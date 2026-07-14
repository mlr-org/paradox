`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

benchmark_read_first_line <- function(path, default = NA_character_) {
  if (!file.exists(path)) return(default)
  value <- tryCatch(readLines(path, n = 1L, warn = FALSE), error = function(...) character())
  if (length(value)) value[[1L]] else default
}

benchmark_command_output <- function(command, args = character()) {
  tryCatch(
    paste(system2(command, args, stdout = TRUE, stderr = TRUE), collapse = "\n"),
    warning = function(condition) paste("warning:", conditionMessage(condition)),
    error = function(condition) paste("error:", conditionMessage(condition))
  )
}

benchmark_r_config <- function(variable) {
  benchmark_command_output(file.path(R.home("bin"), "R"), c("CMD", "config", variable))
}

benchmark_named_value <- function(values, name, default = NA_character_) {
  if (name %in% names(values)) unname(values[[name]]) else default
}

benchmark_package_fingerprint <- function(package_path) {
  files <- sort(list.files(
    package_path,
    all.files = TRUE,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = FALSE,
    no.. = TRUE
  ))
  if (!length(files)) return(NA_character_)
  hashes <- unname(tools::md5sum(files))
  relative <- substring(files, nchar(package_path) + 2L)
  manifest <- tempfile("paradox-installation-manifest-", fileext = ".txt")
  on.exit(unlink(manifest), add = TRUE)
  writeLines(paste(hashes, relative, sep = "  "), manifest, useBytes = TRUE)
  unname(tools::md5sum(manifest))
}

benchmark_host_metadata <- function() {
  governor_files <- Sys.glob("/sys/devices/system/cpu/cpu*/cpufreq/scaling_governor")
  governors <- if (length(governor_files)) {
    unique(vapply(governor_files, benchmark_read_first_line, character(1L)))
  } else {
    NA_character_
  }

  cpuinfo <- if (file.exists("/proc/cpuinfo")) {
    readLines("/proc/cpuinfo", warn = FALSE)
  } else {
    character()
  }
  model <- sub("^[^:]+:[[:space:]]*", "", grep("^(model name|Hardware)[[:space:]]*:", cpuinfo, value = TRUE))
  if (!length(model) && nzchar(Sys.which("sysctl"))) {
    sysctl_model <- benchmark_command_output("sysctl", c("-n", "machdep.cpu.brand_string"))
    if (!startsWith(sysctl_model, "error:") && !startsWith(sysctl_model, "warning:")) {
      model <- sysctl_model
    }
  }

  proc_loadavg <- benchmark_read_first_line("/proc/loadavg")
  load_average <- if (exists("Sys.getloadavg", envir = baseenv(), inherits = FALSE)) {
    unname(Sys.getloadavg())
  } else if (!is.na(proc_loadavg)) {
    fields <- strsplit(proc_loadavg, "[[:space:]]+")[[1L]]
    suppressWarnings(as.numeric(fields[seq_len(min(3L, length(fields)))]))
  } else {
    rep(NA_real_, 3L)
  }
  length(load_average) <- 3L
  names(load_average) <- c("one_minute", "five_minutes", "fifteen_minutes")

  list(
    cpu_model = if (length(model)) model[[1L]] else NA_character_,
    logical_cpus = unname(parallel::detectCores(logical = TRUE)),
    cpu_governors = governors,
    load_average = load_average,
    proc_loadavg = proc_loadavg,
    uname = benchmark_command_output("uname", "-a")
  )
}

benchmark_flatten_allocations <- function(mark, workload_names) {
  pieces <- vector("list", length(workload_names))
  for (i in seq_along(workload_names)) {
    memory <- mark$memory[[i]]
    if (is.null(memory) || !nrow(memory)) next
    trace <- vapply(
      memory$trace,
      function(frames) paste(frames, collapse = " > "),
      character(1L)
    )
    pieces[[i]] <- data.frame(
      workload = workload_names[[i]],
      allocation = seq_len(nrow(memory)),
      what = memory$what,
      bytes = memory$bytes,
      trace = trace,
      stringsAsFactors = FALSE
    )
  }
  nonempty <- vapply(pieces, is.data.frame, logical(1L))
  if (!any(nonempty)) {
    return(data.frame(
      workload = character(), allocation = integer(), what = character(),
      bytes = numeric(), trace = character(), stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, pieces[nonempty])
}

benchmark_flatten_samples <- function(mark, workload_names) {
  pieces <- vector("list", length(workload_names))
  for (i in seq_along(workload_names)) {
    elapsed_seconds <- as.numeric(mark$time[[i]])
    gc_counts <- mark$gc[[i]]
    if (is.null(gc_counts)) {
      gc_counts <- data.frame(
        level0 = rep(NA_integer_, length(elapsed_seconds)),
        level1 = rep(NA_integer_, length(elapsed_seconds)),
        level2 = rep(NA_integer_, length(elapsed_seconds))
      )
    }
    pieces[[i]] <- data.frame(
      workload = workload_names[[i]],
      iteration = seq_along(elapsed_seconds),
      elapsed_seconds = elapsed_seconds,
      elapsed_ns = elapsed_seconds * 1e9,
      gc_level0 = gc_counts$level0,
      gc_level1 = gc_counts$level1,
      gc_level2 = gc_counts$level2,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, pieces)
}

benchmark_summarize <- function(mark, workload_names, samples, allocations) {
  pieces <- vector("list", length(workload_names))
  for (i in seq_along(workload_names)) {
    name <- workload_names[[i]]
    times <- samples$elapsed_ns[samples$workload == name]
    allocation_rows <- allocations[allocations$workload == name, , drop = FALSE]
    quantiles <- stats::quantile(times, c(0, 0.25, 0.5, 0.75, 1), names = FALSE, type = 8)
    pieces[[i]] <- data.frame(
      workload = name,
      iterations = length(times),
      min_ns = quantiles[[1L]],
      q25_ns = quantiles[[2L]],
      median_ns = quantiles[[3L]],
      q75_ns = quantiles[[4L]],
      max_ns = quantiles[[5L]],
      iterations_per_second = 1e9 / quantiles[[3L]],
      mem_alloc_bytes = as.numeric(mark$mem_alloc[[i]]),
      allocation_records = nrow(allocation_rows),
      profiled_allocation_bytes = sum(allocation_rows$bytes, na.rm = TRUE),
      gc_level0 = sum(samples$gc_level0[samples$workload == name], na.rm = TRUE),
      gc_level1 = sum(samples$gc_level1[samples$workload == name], na.rm = TRUE),
      gc_level2 = sum(samples$gc_level2[samples$workload == name], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, pieces)
}

benchmark_worker <- function(
  label,
  target_library,
  output_directory,
  workload_file,
  selected_workloads,
  n_params,
  n_rows,
  iterations,
  warmups,
  seed,
  revision
) {
  stopifnot(label %in% c("baseline", "candidate"))
  target_library <- normalizePath(target_library, winslash = "/", mustWork = TRUE)
  output_directory <- normalizePath(output_directory, winslash = "/", mustWork = TRUE)
  workload_file <- normalizePath(workload_file, winslash = "/", mustWork = TRUE)

  suppressPackageStartupMessages(library(paradox))
  if (!requireNamespace("bench", quietly = TRUE)) {
    stop("The child library path does not provide the 'bench' package.", call. = FALSE)
  }

  package_path <- normalizePath(find.package("paradox"), winslash = "/", mustWork = TRUE)
  expected_path <- normalizePath(
    file.path(target_library, "paradox"),
    winslash = "/",
    mustWork = TRUE
  )
  if (!identical(package_path, expected_path)) {
    stop(
      sprintf("Expected paradox at '%s', but loaded '%s'.", expected_path, package_path),
      call. = FALSE
    )
  }

  source(workload_file, local = environment())
  inputs <- benchmark_make_inputs(n_params, n_rows)
  workloads <- benchmark_make_workloads(inputs)
  workloads <- workloads[selected_workloads]
  if (!identical(names(workloads), selected_workloads)) {
    stop("Unknown workload selected.", call. = FALSE)
  }

  set.seed(seed)
  host_at_process_start <- benchmark_host_metadata()
  started_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)

  validation_keys <- vector("list", length(workloads))
  names(validation_keys) <- names(workloads)
  for (i in seq_along(workloads)) {
    result <- eval(workloads[[i]]$expression, envir = environment())
    validation_keys[[i]] <- workloads[[i]]$validate(result)
  }

  # Explicit warmups keep compilation, lazy loading, and first-use data.table
  # setup outside the recorded distribution.
  for (i in seq_along(workloads)) {
    for (warmup in seq_len(warmups)) {
      eval(workloads[[i]]$expression, envir = environment())
    }
  }

  # Revalidate after warmup to catch accidental mutation of shared fixtures.
  for (i in seq_along(workloads)) {
    result <- eval(workloads[[i]]$expression, envir = environment())
    key <- workloads[[i]]$validate(result)
    if (!identical(key, validation_keys[[i]])) {
      stop(sprintf("Workload '%s' changed its validated result during warmup.", names(workloads)[[i]]))
    }
  }
  rm(result)
  invisible(gc())
  host_at_timing_start <- benchmark_host_metadata()

  expressions <- lapply(workloads, `[[`, "expression")
  benchmark_mark <- bench::mark
  benchmark_call <- as.call(c(
    list(quote(benchmark_mark)),
    expressions,
    list(
      iterations = iterations,
      min_time = 0,
      check = FALSE,
      memory = capabilities("profmem"),
      filter_gc = FALSE
    )
  ))
  mark <- eval(benchmark_call, envir = environment())
  measured_names <- attr(mark$expression, "description", exact = TRUE)
  if (!identical(measured_names, names(workloads))) {
    stop(
      sprintf(
        "bench measured unexpected expressions: expected [%s], observed [%s].",
        paste(names(workloads), collapse = ", "),
        paste(measured_names %||% character(), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # bench omits retained results when check = FALSE. Evaluate once more outside
  # the timed region so every measured expression is still covered by its
  # semantic validator after the complete sample run.
  for (i in seq_along(workloads)) {
    result <- eval(workloads[[i]]$expression, envir = environment())
    key <- workloads[[i]]$validate(result)
    if (!identical(key, validation_keys[[i]])) {
      stop(sprintf("Workload '%s' changed its validated result during timing.", names(workloads)[[i]]))
    }
  }
  rm(result)

  workload_names <- names(workloads)
  samples <- benchmark_flatten_samples(mark, workload_names)
  allocations <- benchmark_flatten_allocations(mark, workload_names)
  summary <- benchmark_summarize(mark, workload_names, samples, allocations)

  utils::write.csv(
    samples,
    file.path(output_directory, sprintf("samples-%s.csv", label)),
    row.names = FALSE,
    na = ""
  )
  utils::write.csv(
    allocations,
    file.path(output_directory, sprintf("allocations-%s.csv", label)),
    row.names = FALSE,
    na = ""
  )
  utils::write.csv(
    summary,
    file.path(output_directory, sprintf("summary-%s.csv", label)),
    row.names = FALSE,
    na = ""
  )

  host_after <- benchmark_host_metadata()
  package_description <- utils::packageDescription("paradox")
  metadata <- list(
    label = label,
    revision = revision,
    started_at_utc = started_at,
    completed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    pid = Sys.getpid(),
    inputs = list(
      n_params = n_params,
      n_rows = n_rows,
      iterations = iterations,
      warmups = warmups,
      seed = seed,
      workloads = workload_names
    ),
    package = list(
      name = "paradox",
      version = as.character(utils::packageVersion("paradox")),
      library = target_library,
      path = package_path,
      built = unname(package_description$Built %||% NA_character_),
      installation_md5 = benchmark_package_fingerprint(package_path)
    ),
    benchmark_package_version = as.character(utils::packageVersion("bench")),
    r = list(
      version = R.version.string,
      platform = R.version$platform,
      arch = R.version$arch,
      os = R.version$os,
      compiler = unname(R.version$compiler %||% NA_character_),
      library_paths = .libPaths(),
      cc = benchmark_r_config("CC"),
      cppflags = benchmark_r_config("CPPFLAGS"),
      cflags = benchmark_r_config("CFLAGS"),
      blas = benchmark_named_value(extSoftVersion(), "BLAS"),
      lapack_version = as.character(La_version()),
      lapack_library = tryCatch(La_library(), error = function(...) NA_character_),
      locale = Sys.getlocale(),
      timezone = Sys.timezone(),
      thread_environment = as.list(Sys.getenv(
        c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS"),
        unset = NA_character_
      ))
    ),
    host_at_process_start = host_at_process_start,
    host_at_timing_start = host_at_timing_start,
    host_after = host_after,
    profmem_available = capabilities("profmem"),
    session_info = paste(capture.output(sessionInfo()), collapse = "\n")
  )

  # Avoid returning large benchmark objects over the process boundary.
  list(metadata = metadata, validation_keys = validation_keys)
}
