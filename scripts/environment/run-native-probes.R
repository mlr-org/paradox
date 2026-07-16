#!/usr/bin/env Rscript

fail <- function(...) stop(..., call. = FALSE)

parse_options <- function(arguments) {
  arguments <- arguments[arguments != "--args"]
  values <- list()
  for (argument in arguments) {
    if (!startsWith(argument, "--") || !grepl("=", argument, fixed = TRUE)) {
      fail("arguments must use --name=value: ", argument)
    }
    pieces <- strsplit(sub("^--", "", argument), "=", fixed = TRUE)[[1L]]
    name <- pieces[[1L]]
    value <- paste(pieces[-1L], collapse = "=")
    if (!nzchar(name) || name %in% names(values)) fail("duplicate or empty option: ", name)
    values[[name]] <- value
  }
  required <- c("mode", "manifest", "candidate-library", "dependency-library", "output")
  missing <- setdiff(required, names(values))
  if (length(missing)) fail("missing options: ", paste(missing, collapse = ", "))
  unknown <- setdiff(names(values), c(required, "expected-dso-sha256", "expected-version", "gct-step", "rounds"))
  if (length(unknown)) fail("unknown options: ", paste(unknown, collapse = ", "))
  values
}

read_manifest <- function(path) {
  manifest <- read.delim(path, sep = "\t", quote = "", comment.char = "",
    colClasses = "character", check.names = FALSE)
  expected_columns <- c("routine", "arity", "surface", "coverage", "probe_ids", "reviewed_basis")
  if (!identical(names(manifest), expected_columns)) fail("coverage manifest columns differ")
  if (!nrow(manifest) || anyNA(manifest) || any(!nzchar(as.matrix(manifest)))) {
    fail("coverage manifest contains an empty value")
  }
  if (anyDuplicated(manifest$routine)) fail("coverage manifest has duplicate routines")
  arity <- suppressWarnings(as.integer(manifest$arity))
  if (anyNA(arity) || any(as.character(arity) != manifest$arity) || any(arity < 0L)) {
    fail("coverage manifest has invalid arities")
  }
  manifest$arity <- arity
  manifest
}

safe_detail <- function(value) {
  value <- paste(value, collapse = "; ")
  value <- gsub("[\t\r\n]+", " ", value)
  substr(value, 1L, 1000L)
}

write_result <- function(rows, path) {
  directory <- dirname(path)
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE, mode = "0700")
  temporary <- paste0(path, ".new.", Sys.getpid())
  on.exit(unlink(temporary), add = TRUE)
  table <- do.call(rbind, lapply(rows, function(row) {
    data.frame(kind = row[[1L]], id = row[[2L]], status = row[[3L]],
      detail = safe_detail(row[[4L]]), stringsAsFactors = FALSE)
  }))
  write.table(table, temporary, sep = "\t", quote = FALSE, row.names = FALSE,
    col.names = TRUE, na = "")
  if (!file.rename(temporary, path)) fail("could not publish result: ", path)
}

check <- function(condition, message) {
  if (length(condition) != 1L || is.na(condition) || !isTRUE(condition)) fail(message)
  invisible(TRUE)
}

main <- function() {
  options <- parse_options(commandArgs(trailingOnly = TRUE))
  previous_warning_option <- getOption("warn")
  base::options(warn = 2L)
  on.exit(base::options(warn = previous_warning_option), add = TRUE)
  mode <- options$mode
  if (!mode %in% c("plain", "gct", "valgrind")) fail("unsupported mode: ", mode)
  rounds <- suppressWarnings(as.integer(options$rounds %||% "1"))
  gct_step <- suppressWarnings(as.integer(options$`gct-step` %||% "10"))
  if (is.na(rounds) || rounds < 1L || rounds > 100L) fail("rounds must be in [1, 100]")
  if (is.na(gct_step) || gct_step < 1L) fail("gct-step must be positive")

  manifest_path <- normalizePath(options$manifest, mustWork = TRUE)
  candidate_library <- normalizePath(options$`candidate-library`, mustWork = TRUE)
  dependency_library <- normalizePath(options$`dependency-library`, mustWork = TRUE)
  output <- normalizePath(options$output, mustWork = FALSE)
  manifest <- read_manifest(manifest_path)

  .libPaths(unique(c(candidate_library, dependency_library, .Library)))
  suppressPackageStartupMessages(library("paradox", lib.loc = candidate_library,
    character.only = TRUE))
  package_path <- normalizePath(find.package("paradox", lib.loc = candidate_library), mustWork = TRUE)
  check(identical(dirname(package_path), candidate_library), "paradox was not loaded from the candidate library")
  version <- as.character(packageVersion("paradox", lib.loc = candidate_library))
  expected_version <- options$`expected-version` %||% "2.0.0"
  check(identical(version, expected_version), "candidate version differs")

  dll <- getLoadedDLLs()[["paradox"]]
  check(!is.null(dll), "paradox DSO is not loaded")
  expected_dso <- normalizePath(file.path(package_path, "libs",
    paste0("paradox", .Platform$dynlib.ext)), mustWork = TRUE)
  actual_dso <- normalizePath(dll[["path"]], mustWork = TRUE)
  check(identical(actual_dso, expected_dso), "loaded paradox DSO path differs")
  dso_sha256 <- unname(tools::sha256sum(actual_dso))
  if (!is.null(options$`expected-dso-sha256`)) {
    check(grepl("^[0-9a-f]{64}$", options$`expected-dso-sha256`), "expected DSO hash is malformed")
    check(identical(dso_sha256, options$`expected-dso-sha256`), "loaded paradox DSO hash differs")
  }
  check(identical(dll[["dynamicLookup"]], FALSE), "dynamic symbol lookup is enabled")
  check(identical(dll[["forceSymbols"]], TRUE), "registered symbols are not forced")

  registered <- getDLLRegisteredRoutines(dll)
  check(!length(registered$.C) && !length(registered$.Fortran) &&
    !length(registered$.External), "unexpected non-.Call registration")
  calls <- registered$.Call
  observed_names <- names(calls)
  observed_arities <- unname(vapply(calls, function(entry) entry$numParameters, integer(1L)))
  check(identical(observed_names, manifest$routine), "registered .Call name/order inventory differs")
  check(identical(observed_arities, manifest$arity), "registered .Call arity inventory differs")

  rows <- list()
  add <- function(kind, id, status = "pass", detail = "ok") {
    rows[[length(rows) + 1L]] <<- list(kind, id, status, detail)
  }
  add("meta", "schema", detail = "1")
  add("meta", "mode", detail = mode)
  add("meta", "rounds", detail = as.character(rounds))
  add("meta", "gct_step", detail = if (mode == "gct") as.character(gct_step) else "disabled")
  add("meta", "candidate_version", detail = version)
  add("meta", "dso_sha256", detail = dso_sha256)
  add("meta", "dynamic_lookup", detail = "false")
  add("meta", "force_symbols", detail = "true")
  for (index in seq_along(calls)) {
    add("registration", observed_names[[index]], detail = paste0("arity=", observed_arities[[index]]))
  }

  symbol <- function(name) calls[[name]]
  namespace <- asNamespace("paradox")
  private_of <- function(object) object$.__enclos_env__$private
  bind_null <- function(environment, names) {
    for (name in names) assign(name, NULL, envir = environment)
    environment
  }
  stateful <- function(first, later = first, elt_switch_after = NA_integer_,
      length_switch_after = NA_integer_, callback = NULL,
      callback_after = NA_integer_) {
    .Call(symbol("test_stateful_altrep"), first, later,
      as.integer(elt_switch_after), as.integer(length_switch_after), callback,
      as.integer(callback_after))[[1L]]
  }
  plain_table <- function(values = list(x = c(1L, 2L))) {
    structure(values, names = names(values), row.names = .set_row_names(length(values[[1L]])),
      class = c("data.table", "data.frame"))
  }
  values_frame <- function() {
    frame <- new.env(parent = namespace)
    bind_null(frame, c("class", "tags", "any_tags"))
    assign("type", "with_token", envir = frame)
    assign("check_required", TRUE, envir = frame)
    assign("remove_dependencies", TRUE, envir = frame)
    frame
  }
  ids_frame <- function() {
    frame <- new.env(parent = namespace)
    bind_null(frame, c("class", "tags", "any_tags"))
  }
  subset_private <- function() {
    list2env(list(.params = NULL, .tags = NULL, .trafos = NULL, .deps = NULL,
      .values = NULL, .extra_trafo = NULL), parent = emptyenv())
  }
  probes <- list(
    direct_design_transpose = function() {
      result <- .Call(symbol("design_transpose"), list(x = c(1L, 2L), y = c(3, 4)), FALSE)
      check(identical(result, list(list(x = 1L, y = 3), list(x = 2L, y = 4))),
        "transpose result differs")
    },
    direct_design_transpose_logscale_builtin = function() {
      set <- ps(x = p_dbl(1, 10, logscale = TRUE))
      result <- .Call(symbol("design_transpose_logscale_builtin"),
        list(list(x = 0)), set)
      check(identical(result, list(list(x = 1))),
        "logscale transpose result differs")
    },
    direct_design_dependency_runtime = function() {
      result <- .Call(symbol("design_dependency_runtime"),
        getNamespaceVersion("mlr3misc")[[1L]])
      check(isTRUE(result), "canonical dependency runtime was rejected")
    },
    direct_design_dependency_plan_builtin = function() {
      set <- ps(parent = p_lgl(), child = p_int(0L, 9L))
      set$add_dep("child", "parent", CondEqual(TRUE))
      data <- data.table::data.table(
        parent = c(TRUE, FALSE),
        child = c(1L, 2L)
      )
      result <- .Call(symbol("design_dependency_plan_builtin"), data, set)
      check(identical(result, list(
        rows = list(2L), columns = "child", values = list(NA_integer_)
      )), "dependency plan differs")
    },
    direct_finalize_data_table = function() {
      input <- plain_table()
      result <- .Call(symbol("finalize_data_table"), input)
      check(inherits(result, "data.table") && identical(result$x, c(1L, 2L)),
        "finalized shell content/class differs")
      check(data.table:::selfrefok(result, verbose = FALSE) == 1L, "finalized shell has invalid selfref")
    },
    direct_domain_check_builtin = function() {
      check(isTRUE(.Call(symbol("domain_check_builtin"), p_int(0L, 2L), list(1L))), "domain check failed")
    },
    direct_domain_construct = function() {
      result <- .Call(symbol("domain_construct"), "ParamDbl", "ParamDbl", NULL,
        0, 1, 0, NULL, list(), get("NO_DEF", namespace), character(), NULL,
        "numeric", FALSE, NULL)
      check(identical(names(result), get("domain_names", namespace)) &&
        identical(result$cls, "ParamDbl") && identical(result$grouping, "ParamDbl") &&
        identical(result$lower, 0) && identical(result$upper, 1) &&
        identical(result$storage_type, "numeric") && identical(result$.init_given, FALSE),
        "domain constructor result differs")
    },
    direct_domain_construct_frame = function() {
      constructor <- function(cls = "ParamLgl", grouping = "ParamLgl", cargo = list(),
          lower = NA_real_, upper = NA_real_, tolerance = NA_real_,
          levels = c(TRUE, FALSE), special_vals = list(),
          default = stop("default promise forced", call. = FALSE), tags = character(),
          trafo = NULL, depends_expr = stop("depends promise forced", call. = FALSE),
          storage_type = stop("storage promise forced", call. = FALSE),
          init = stop("init promise forced", call. = FALSE)) {
        .Call(symbol("domain_construct_frame"), environment())
      }
      result <- constructor()
      check(length(result) == 2L && identical(result[[2L]], "logical") &&
        identical(result[[1L]]$cls, "ParamLgl") &&
        identical(result[[1L]]$storage_type, "logical") &&
        identical(result[[1L]]$.init_given, FALSE),
        "frame constructor result differs")
    },
    direct_domain_builtin_runtime = function() {
      constructors <- list(
        p_dbl = get("p_dbl", namespace),
        p_int = get("p_int", namespace),
        p_fct = get("p_fct", namespace),
        p_lgl = get("p_lgl", namespace)
      )
      result <- .Call(symbol("domain_builtin_runtime"), constructors,
        get("NO_DEF", namespace), base::sort)
      check(isTRUE(result), "canonical Domain runtime was rejected")
    },
    direct_domain_construct_builtin = function() {
      p_dbl <- get("p_dbl", namespace, inherits = FALSE)
      caller <- new.env(parent = baseenv())
      assign("p_dbl", p_dbl, envir = caller)
      result <- eval(quote(p_dbl(0, 1)), envir = caller)
      check(inherits(result, "ParamDbl") && identical(result$lower, 0) &&
        identical(result$upper, 1) &&
        identical(attr(result, "repr", exact = TRUE),
          quote(p_dbl(lower = 0, upper = 1))),
        "direct built-in Domain differs")
    },
    direct_domain_fct_grouping = function() {
      result <- .Call(symbol("domain_fct_grouping"), c('a"b', "c\\d"))
      check(identical(result, '"a\\"b","c\\\\d"'),
        "factor grouping escape result differs")
    },
    direct_domain_numeric_bounds_admit = function() {
      constructor <- function(
          tolerance = 0, lower = -1, upper = 1, logscale = FALSE) {
        .Call(
          symbol("domain_numeric_bounds_admit"),
          environment(),
          FALSE
        )
      }
      check(isTRUE(constructor()), "numeric bounds admission declined")
    },
    direct_domain_uty_check_result = function() {
      check(isTRUE(.Call(symbol("domain_uty_check_result"), "diagnostic")) &&
        identical(.Call(symbol("domain_uty_check_result"), FALSE), FALSE),
        "utility-check result admission differs")
    },
    direct_domain_simple_repr_id = function() {
      check(identical(.Call(symbol("domain_simple_repr_id"), quote(p_dbl())),
          "p_dbl()") &&
        is.null(.Call(symbol("domain_simple_repr_id"), quote(p_dbl(0, 1)))) &&
        is.null(.Call(symbol("domain_simple_repr_id"), quote(paradox::p_dbl()))),
        "simple Domain representation admission differs")
    },
    direct_ps_builtin_runtime = function() {
      constructors <- list(
        p_dbl = get("p_dbl", namespace),
        p_int = get("p_int", namespace),
        p_fct = get("p_fct", namespace),
        p_lgl = get("p_lgl", namespace)
      )
      result <- .Call(symbol("ps_builtin_runtime"), constructors,
        get("NO_DEF", namespace))
      check(isTRUE(result), "canonical ps runtime was rejected")
    },
    direct_ps_builtin_domains = function() {
      result <- .Call(symbol("ps_builtin_domains"),
        quote(list(x = p_int(0L, 2L))), environment())
      check(identical(names(result), "x") && inherits(result[[1L]], "ParamInt") &&
        identical(result[[1L]]$id, "x") &&
        identical(result[[1L]]$lower, 0L) &&
        identical(result[[1L]]$upper, 2L), "native ps Domain list differs")
    },
    direct_domain_qunif_builtin = function() {
      check(identical(.Call(symbol("domain_qunif_builtin"), p_dbl(0, 10), c(0, .5, 1)), c(0, 5, 10)), "domain qunif differs")
    },
    direct_domain_sanitize_builtin = function() {
      check(identical(.Call(symbol("domain_sanitize_builtin"), p_dbl(0, 1, tolerance = .1), list(-.05, 1.05)), list(0, 1)), "domain sanitize differs")
    },
    direct_param_set_index_layout = function() {
      params_probe <- data.table::data.table(
        id = c("z", "a", "m"),
        cls = rep("ParamDbl", 3L),
        grouping = rep("ParamDbl", 3L)
      )
      data.table::setindexv(params_probe, c("id", "cls", "grouping"))
      identity_probe <- data.table::data.table(
        id = c("a", "m", "z"),
        cls = rep("ParamDbl", 3L),
        grouping = rep("ParamDbl", 3L)
      )
      data.table::setindexv(identity_probe, c("id", "cls", "grouping"))
      tags_probe <- data.table::data.table(tag = c("a", "B", "", "_", "b", "A", "a"))
      data.table::setindexv(tags_probe, "tag")
      empty_probe <- data.table::data.table(tag = character())
      data.table::setindexv(empty_probe, "tag")
      result <- .Call(
        symbol("param_set_index_layout"),
        getNamespaceVersion("data.table")[[1L]],
        attr(params_probe, "index", exact = TRUE),
        attr(tags_probe, "index", exact = TRUE),
        attr(identity_probe, "index", exact = TRUE),
        attr(empty_probe, "index", exact = TRUE)
      )
      check(isTRUE(result), "reviewed data.table index layout was rejected")
    },
    direct_param_set_construct = function() {
      result <- .Call(symbol("param_set_construct"), list(x = p_int(0L, 2L), y = p_lgl()))
      check(identical(result$params$id, c("x", "y")), "ParamSet construction differs")
    },
    direct_param_set_collection_construct = function() {
      child <- ps(x = p_int())
      result <- .Call(symbol("param_set_collection_construct"), list(owner = child), TRUE, TRUE, FALSE)
      check(identical(result$params$id, "owner.x"), "collection construction differs")
    },
    direct_param_set_collection_detach_plan = function() {
      child <- ps(x = p_int(), y = p_lgl())
      child$constraint <- function(x) TRUE
      collection <- psc(owner = child)
      result <- .Call(symbol("param_set_collection_detach_plan"),
        private_of(collection), collection, "owner.x")
      check(identical(result$translation$id, "owner.x") &&
        identical(result$translation$original_id, "x") &&
        identical(result$constraint_indices, 1L) &&
        identical(names(result$constraint_sets), "owner"),
        "collection detachment plan differs")
    },
    direct_param_set_collection_check_builtin = function() {
      collection <- psc(owner = ps(x = p_int(0L, 2L)))
      result <- .Call(symbol("param_set_collection_check_builtin"),
        private_of(collection), collection, list(owner.x = 1L), FALSE, TRUE)
      check(isTRUE(result), "collection scalar check failed")
    },
    direct_param_set_check_builtin = function() {
      set <- ps(x = p_int(0L, 2L))
      result <- .Call(symbol("param_set_check_builtin"), private_of(set)$.params, list(x = 1), TRUE)
      check(isTRUE(result) && identical(attr(result, "sanitized"), list(x = 1L)), "ParamSet scalar check differs")
    },
    direct_param_set_check_dt_builtin = function() {
      set <- ps(x = p_int(0L, 2L))
      check(isTRUE(.Call(symbol("param_set_check_dt_builtin"), private_of(set)$.params,
        data.frame(x = c(0L, 2L)))), "ParamSet table check differs")
    },
    direct_param_set_check_dt_plan_builtin = function() {
      set <- ps(x = p_int(0L, 2L), y = p_lgl())
      result <- .Call(symbol("param_set_check_dt_plan_builtin"),
        private_of(set)$.params,
        data.frame(x = c(0L, 2L), y = c(TRUE, FALSE)))
      check(identical(result, 3L), "ParamSet table plan differs")
    },
    direct_param_set_check_dt_complete_builtin = function() {
      set <- ps(x = p_int(0L, 2L), y = p_lgl())
      check(isTRUE(.Call(symbol("param_set_check_dt_complete_builtin"),
        private_of(set)$.params, data.frame(x = c(0L, 2L)))),
        "ParamSet complete-cell table check differs")
    },
    direct_param_set_check_dt_all_builtin = function() {
      set <- ps(x = p_int(0L, 2L), y = p_lgl())
      check(isTRUE(.Call(symbol("param_set_check_dt_all_builtin"),
        private_of(set)$.params,
        data.frame(x = c(0L, 2L), y = c(TRUE, FALSE)))),
        "ParamSet complete all-parameter table check differs")
    },
    direct_param_set_surface_auth = function() {
      set <- ps(x = p_int())
      observed <- vapply(1:4, function(surface) .Call(symbol("param_set_surface_auth"), set, as.integer(surface)), logical(1L))
      check(all(observed), "one or more canonical ParamSet surfaces were rejected")
    },
    direct_param_set_ids = function() {
      set <- ps(x = p_int(tags = c("red", "fast")), y = p_dbl(tags = "red"))
      private <- private_of(set)
      result <- .Call(symbol("param_set_ids"), private$.params, private$.tags, NULL, c("red", "fast"), NULL)
      check(identical(result, "x"), "ParamSet ids filter differs")
    },
    direct_param_set_ids_lazy = function() {
      set <- ps(x = p_int(), y = p_dbl())
      check(identical(.Call(symbol("param_set_ids_lazy"), private_of(set), ids_frame()), c("x", "y")), "lazy ids differ")
    },
    direct_param_set_get_values = function() {
      set <- ps(x = p_int(init = 1L))
      check(identical(.Call(symbol("param_set_get_values"), private_of(set), set, values_frame()), list(x = 1L)), "get_values differs")
    },
    direct_param_set_values_merge = function() {
      result <- .Call(symbol("param_set_values_merge"), list(a = 1L), list(b = 2L), NULL, FALSE)
      check(identical(result, list(a = 1L, b = 2L)), "values merge differs")
    },
    direct_param_set_store_values = function() {
      set <- ps(a = p_int(), b = p_int())
      private <- private_of(set)
      check(identical(.row_names_info(private$.params, 0L), c(NA_integer_, -2L)),
        "ParamSet probe lacks compact row names")
      result <- .Call(symbol("param_set_store_values"), private, set, list(b = 2L, a = 1L))
      check(identical(result, list(a = 1L, b = 2L)), "stored values differ")
    },
    direct_param_set_assign_values_checked = function() {
      set <- ps(x = p_int(0L, 2L))
      result <- .Call(symbol("param_set_assign_values_checked"), private_of(set), set, list(x = 1))
      check(identical(result, list(x = 1L)), "checked assignment differs")
    },
    direct_param_set_collection_store_plan = function() {
      child <- ps(x = p_int())
      collection <- ParamSetCollection$new(list(owner = child))
      private <- private_of(collection)
      result <- .Call(symbol("param_set_collection_store_plan"), private, collection,
        private$.sets, list(owner.x = 1L))
      check(identical(result[[1L]], 1L) && identical(result[[2L]][[1L]], list(x = 1L)), "collection store plan differs")
    },
    direct_param_set_property = function() {
      set <- ps(x = p_int(0L, 2L))
      result <- .Call(symbol("param_set_property"), private_of(set)$.params, 0L)
      check(length(result) == 2L && identical(result[[1L]], c(x = 3)) &&
        identical(result[[2L]], TRUE), "property result differs")
    },
    direct_param_set_qunif_builtin = function() {
      set <- ps(x = p_int(0L, 10L), y = p_dbl(0, 1))
      units <- matrix(c(.5, .25), nrow = 1L, dimnames = list(NULL, c("x", "y")))
      result <- .Call(symbol("param_set_qunif_builtin"), private_of(set)$.params, units)
      check(identical(result$x, 5L) && identical(result$y, .25), "ParamSet qunif differs")
    },
    direct_sampler_unif_sample_builtin = function() {
      imports <- parent.env(namespace)
      providers <- list(
        runif = asNamespace("stats"),
        data.table = asNamespace("data.table"),
        setnames = asNamespace("data.table"),
        map_dtc = asNamespace("mlr3misc")
      )
      for (name in names(providers)) {
        invisible(get(name, imports, inherits = FALSE))
        invisible(get(name, providers[[name]], inherits = FALSE))
      }
      caller <- new.env(parent = baseenv())
      for (name in c("ps", "p_int", "p_dbl", "p_fct", "p_lgl")) {
        assign(name, get(name, namespace, inherits = FALSE), envir = caller)
      }
      set <- eval(quote(ps(
        x = p_int(0L, 10L),
        y = p_dbl(0, 1),
        z = p_fct(c("left", "right")),
        flag = p_lgl()
      )), envir = caller)
      sampler <- SamplerUnif$new(set)
      sampler$sample(0L)
      set.seed(1729L)
      result <- .Call(
        symbol("sampler_unif_sample_builtin"),
        sampler,
        sampler$param_set,
        sampler$samplers,
        3L
      )
      seed_after <- serialize(.Random.seed, NULL, version = 2L)
      set.seed(1729L)
      seed_before <- serialize(.Random.seed, NULL, version = 2L)
      rng_changed <- !identical(seed_after, seed_before)
      valid <- inherits(result, "data.table") &&
        identical(dim(result), c(3L, 4L)) &&
        identical(names(result), c("x", "y", "z", "flag")) &&
        rng_changed
      if (!valid) {
        fail("bulk SamplerUnif result differs: type=", typeof(result),
          ";class=", paste(class(result), collapse = ","),
          ";dim=", paste(dim(result), collapse = ","),
          ";names=", paste(names(result), collapse = ","),
          ";rng_changed=", rng_changed)
      }
    },
    direct_generate_design_grid_builtin = function() {
      set <- ps(x = p_int(0L, 2L), y = p_dbl(0, 1))
      result <- .Call(
        symbol("generate_design_grid_builtin"),
        private_of(set)$.params,
        c(y = 3, x = 3)
      )
      check(
        identical(names(result), c("y", "x")) &&
          identical(result$y, rep(c(0, .5, 1), each = 3L)) &&
          identical(result$x, rep(0:2, times = 3L)) &&
          data.table:::selfrefok(result, verbose = FALSE) == 1L,
        "one-shot grid differs"
      )
    },
    direct_param_set_trafo_plan = function() {
      set <- ps(x = p_dbl(0, 1, trafo = function(value) value * 2))
      result <- .Call(symbol("param_set_trafo_plan"), list(x = .25), private_of(set)$.trafos)
      check(identical(result[[1L]], "x") && identical(result[[3L]][[1L]], .25) &&
        identical(result[[2L]][[1L]](result[[3L]][[1L]]), .5), "trafo plan differs")
    },
    direct_param_set_get_domain = function() {
      set <- ps(x = p_int())
      result <- .Call(symbol("param_set_get_domain"), private_of(set), set, "x")
      check(inherits(result, "Domain") && identical(result$id, "x"), "get_domain differs")
    },
    direct_param_set_domains = function() {
      set <- ps(x = p_int(), y = p_lgl())
      result <- .Call(symbol("param_set_domains"), private_of(set), set)
      check(identical(names(result), c("x", "y")), "domains result differs")
    },
    direct_param_set_params = function() {
      set <- ps(x = p_int(), y = p_lgl())
      result <- .Call(symbol("param_set_params"), private_of(set), set)
      check(identical(result$id, c("x", "y")) && data.table:::selfrefok(result, FALSE) == 1L, "params result differs")
    },
    direct_param_set_collection_params = function() {
      collection <- ParamSetCollection$new(list(owner = ps(x = p_int())))
      result <- .Call(symbol("param_set_collection_params"), private_of(collection), collection)
      check(identical(result$id, "owner.x"), "collection params differ")
    },
    direct_param_set_collection_deps = function() {
      child <- ps(a = p_int(init = 1L), b = p_int(init = 2L))
      child$add_dep("b", "a", CondEqual(1L))
      collection <- ParamSetCollection$new(list(owner = child))
      result <- .Call(symbol("param_set_collection_deps"), private_of(collection), collection)
      check(identical(result$id, "owner.b") && identical(result$on, "owner.a"), "collection deps differ")
    },
    direct_param_set_collection_values = function() {
      collection <- ParamSetCollection$new(list(owner = ps(x = p_int(init = 1L))))
      private <- private_of(collection)
      check(identical(.row_names_info(private$.params, 0L), c(NA_integer_, -1L)),
        "ParamSetCollection probe lacks compact row names")
      result <- .Call(symbol("param_set_collection_values"), private, collection)
      check(identical(result, list(owner.x = 1L)), "collection values differ")
    },
    direct_param_set_subset_state = function() {
      set <- ps(x = p_int(), y = p_lgl())
      result <- .Call(symbol("param_set_subset_state"), private_of(set), set, "y", FALSE)
      check(identical(result$missing_parents, character()) &&
        typeof(result$state) == "externalptr", "subset transaction differs")
    },
    direct_param_set_subspace_state = function() {
      set <- ps(x = p_int(init = 1L), y = p_lgl(init = TRUE))
      values <- set$values
      result <- .Call(symbol("param_set_subspace_state"), private_of(set),
        set, "x", NA, values)
      check(identical(result$missing_parents, character()) &&
        typeof(result$state) == "externalptr", "subspace transaction differs")
    },
    direct_param_set_subspace_states = function() {
      set <- ps(x = p_int(init = 1L), y = p_lgl(init = TRUE))
      values <- set$values
      result <- .Call(symbol("param_set_subspace_states"), private_of(set),
        set, c("x", "y"), values)
      check(identical(names(result), c("x", "y")) &&
        all(vapply(result, function(plan) {
          identical(plan$missing_parents, character()) &&
            typeof(plan$state) == "externalptr"
        }, logical(1L))), "bulk subspace transactions differ")
    },
    direct_param_set_adopt_subset_state = function() {
      set <- ps(x = p_int(), y = p_lgl())
      plan <- .Call(symbol("param_set_subset_state"), private_of(set), set, "y", FALSE)
      target <- subset_private()
      check(isTRUE(.Call(symbol("param_set_adopt_subset_state"), target, plan$state)), "subset adoption failed")
      check(identical(target$.params$id, "y"), "adopted subset differs")
    },
    direct_param_set_bulk_shell_register = function() {
      generator <- get("ParamSet", namespace, inherits = FALSE)
      check(isTRUE(.Call(symbol("param_set_bulk_generator_auth"), generator)),
        "load-time ParamSet shell registration was not successful")
      result <- .Call(symbol("param_set_bulk_shell_register"),
        generator$new(), generator)
      check(identical(result, FALSE),
        "repeated ParamSet shell registration was not rejected")
    },
    direct_param_set_bulk_generator_auth = function() {
      generator <- get("ParamSet", namespace, inherits = FALSE)
      check(isTRUE(.Call(symbol("param_set_bulk_generator_auth"), generator)),
        "canonical ParamSet generator was rejected")
    },
    direct_param_set_bulk_shells = function() {
      set <- ps(x = p_int(init = 1L), y = p_lgl(init = TRUE))
      values <- set$values
      plans <- .Call(symbol("param_set_subspace_states"), private_of(set),
        set, c("x", "y"), values)
      result <- .Call(symbol("param_set_bulk_shells"),
        get("ParamSet", namespace, inherits = FALSE), plans)
      check(identical(names(result), c("x", "y")) &&
        all(vapply(result, inherits, logical(1L), "ParamSet")) &&
        identical(result[[1L]]$ids(), "x") &&
        identical(result[[2L]]$ids(), "y"),
        "bulk ParamSet shells differ")
    },
    direct_sampler_1d_unif_bulk_register = function() {
      generators <- get("sampler_1d_unif_generators", namespace,
        inherits = FALSE)()
      check(isTRUE(.Call(symbol("sampler_1d_unif_bulk_auth"), generators)),
        "load-time Sampler1DUnif shell registration was not successful")
      prototype <- Sampler1DUnif$new(ps(probe = p_lgl()))
      result <- .Call(symbol("sampler_1d_unif_bulk_register"),
        prototype, generators)
      check(identical(result, FALSE),
        "repeated Sampler1DUnif shell registration was not rejected")
    },
    direct_sampler_1d_unif_bulk_auth = function() {
      generators <- get("sampler_1d_unif_generators", namespace,
        inherits = FALSE)()
      check(isTRUE(.Call(symbol("sampler_1d_unif_bulk_auth"), generators)),
        "canonical Sampler1DUnif generators were rejected")
    },
    direct_sampler_1d_unif_bulk_shells = function() {
      set <- ps(x = p_int(0L, 2L, init = 1L), y = p_lgl(init = TRUE))
      values <- set$values
      plans <- .Call(symbol("param_set_subspace_states"), private_of(set),
        set, c("x", "y"), values)
      generators <- get("sampler_1d_unif_generators", namespace,
        inherits = FALSE)()
      result <- .Call(symbol("sampler_1d_unif_bulk_shells"),
        get("ParamSet", namespace, inherits = FALSE), generators, plans, values)
      check(identical(names(result), c("x", "y")) &&
        all(vapply(result, inherits, logical(1L), "Sampler1DUnif")) &&
        identical(result[[1L]]$param$ids(), "x") &&
        identical(result[[2L]]$param$ids(), "y"),
        "bulk Sampler1DUnif shells differ")
    },
    direct_test_checked_affixed_size = function() {
      check(identical(.Call(symbol("test_checked_affixed_size"), 0L, 0L), 1L), "affix boundary result differs")
    },
    direct_test_stateful_altrep = function() {
      value <- .Call(symbol("test_stateful_altrep"), c(1L, 2L), c(3L, 4L),
        1L, NA_integer_, NULL, NA_integer_)[[1L]]
      observed <- vapply(seq_along(value), function(index) value[[index]], integer(1L))
      check(identical(observed, c(1L, 4L)), "stateful ALTREP transition differs")
    },
    direct_test_stateful_altrep_rearm = function() {
      callbacks <- 0L
      value <- stateful(c(1L, 2L), c(1L, 2L), callback = function() {
        callbacks <<- callbacks + 1L
        invisible(gc())
      })
      return_value <- .Call(symbol("test_stateful_altrep_rearm"), value, 0L)
      observed <- value[[1L]]
      check(is.null(return_value) && identical(observed, 1L) &&
        identical(callbacks, 1L), "ALTREP rearm operation differs")
    },
    direct_test_gc_column_mutator = function() {
      table <- list(x = 1L)
      pointer <- .Call(symbol("test_gc_column_mutator"), table, 0L, 2L)
      check(typeof(pointer) == "externalptr", "GC mutator did not return an external pointer")
    }
  )

  hazards <- list(
    hazard_altrep_snapshot_reentry = function() {
      callbacks <- 0L
      changing <- stateful(list(a = 1L, b = 2L), list(a = 10L, b = 20L),
        elt_switch_after = 1L, callback = function() {
          callbacks <<- callbacks + 1L
          invisible(.Call(symbol("domain_qunif_builtin"), p_dbl(0, 1), .5))
          invisible(gc())
        }, callback_after = 0L)
      result <- .Call(symbol("param_set_values_merge"), changing, list(c = 3L), NULL, FALSE)
      check(identical(result, list(a = 1L, b = 20L, c = 3L)), "ALTREP snapshot result differs")
      check(identical(callbacks, 1L), "ALTREP callback count differs")
    },
    hazard_callback_reentry_rooting = function() {
      callbacks <- 0L
      domain <- p_dbl(0, 1, tolerance = .1)
      value <- stateful(-.05, -.05, callback = function() {
        callbacks <<- callbacks + 1L
        data.table::set(domain, i = 1L, j = "lower", value = 100)
        data.table::set(domain, i = 1L, j = "upper", value = 200)
        invisible(.Call(symbol("domain_qunif_builtin"), p_int(0L, 2L), .5))
        invisible(gc())
      }, callback_after = 0L)
      check(isTRUE(.Call(symbol("domain_check_builtin"), domain, list(value))), "callback-rooted domain check differs")
      check(identical(callbacks, 1L), "callback-rooting count differs")
    },
    hazard_finalizer_column_mutation = function() {
      table <- list(x = 1L)
      pointer <- .Call(symbol("test_gc_column_mutator"), table, 0L, 99L)
      pointer <- NULL
      for (index in 1:3) invisible(gc(full = TRUE))
      check(identical(table[[1L]], 99L), "registered C finalizer did not run")
    },
    hazard_finalize_names_alias = function() {
      input <- plain_table()
      result <- .Call(symbol("finalize_data_table"), input)
      data.table::setnames(result, "x", "renamed")
      check(identical(names(input), "x"), "finalization retained a shared names vector")
      check(identical(names(result), "renamed"), "finalized table rename failed")
    }
  )

  manifest_probes <- unique(unlist(strsplit(manifest$probe_ids, ",", fixed = TRUE), use.names = FALSE))
  known <- c(names(probes), names(hazards))
  check(!anyDuplicated(known), "runner has duplicate probe IDs")
  check(setequal(manifest_probes, known), "manifest and runner probe inventories differ")

  old_gct <- NULL
  restore_gct <- FALSE
  if (mode == "gct") {
    old_gct <- gctorture2(gct_step)
    restore_gct <- TRUE
    on.exit(if (restore_gct) gctorture2(old_gct), add = TRUE)
    check(identical(old_gct, 0L), "GCT was already active before the targeted probe scope")
  }
  execute <- function(kind, id, function_) {
    error <- NULL
    tryCatch(withCallingHandlers({
      for (round in seq_len(rounds)) function_()
    }, warning = function(condition) {
      if (is.null(error)) error <<- paste0("warning: ", conditionMessage(condition))
      invokeRestart("muffleWarning")
    }), error = function(condition) error <<- conditionMessage(condition))
    if (is.null(error)) add(kind, id, "pass", paste0("rounds=", rounds))
    else add(kind, id, "fail", error)
  }
  for (id in names(probes)) execute("probe", id, probes[[id]])
  for (id in names(hazards)) execute("hazard", id, hazards[[id]])
  if (mode == "gct") {
    active_step <- gctorture2(old_gct)
    restore_gct <- FALSE
    restored_step <- gctorture2(old_gct)
    check(identical(active_step, gct_step) && identical(restored_step, old_gct),
      "GCT activation/restoration transition differs")
    add("gct", "transition", "pass", paste0(
      "previous=", old_gct, ";active=", active_step, ";restored=", restored_step))
  }

  failures <- sum(vapply(rows, function(row) identical(row[[3L]], "fail"), logical(1L)))
  add("summary", "complete", if (failures) "fail" else "pass",
    paste0("records_before_summary=", length(rows), ";failures=", failures))
  write_result(rows, output)
  if (failures) 1L else 0L
}

`%||%` <- function(left, right) if (is.null(left)) right else left

status <- tryCatch(main(), error = function(condition) {
  message("run-probes: ", conditionMessage(condition))
  2L
})
quit(save = "no", status = status, runLast = FALSE)
