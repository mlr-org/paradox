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
  state_of <- function(object) {
    .Call(symbol("param_set_core_state"), private_of(object))
  }
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
    list2env(list(.core = NULL), parent = emptyenv())
  }
  probes <- list(
    direct_param_set_core_new = function() {
      set <- ps(x = p_int(init = 1L))
      result <- .Call(symbol("param_set_core_new"), 1L, state_of(set))
      check(typeof(result) == "externalptr" &&
        identical(.Call(symbol("param_set_core_kind"), result), 1L),
        "BASE capsule construction differs")
    },
    direct_param_set_core_state = function() {
      set <- ps(x = p_int(init = 1L))
      result <- .Call(symbol("param_set_core_state"), private_of(set))
      check(identical(names(result), c(
        ".params", ".values", ".tags", ".deps", ".trafos",
        ".extra_trafo", ".constraint", ".sets", ".translation", ".postfix"
      )) && identical(result$.values, list(x = 1L)),
      "capsule payload recovery differs")
    },
    direct_param_set_core_replace = function() {
      set <- ps(x = p_int())
      private <- private_of(set)
      before_address <- data.table::address(private$.core)
      result <- .Call(
        symbol("param_set_core_replace"),
        private,
        list(.values = list(x = 2L))
      )
      check(typeof(result) == "externalptr" &&
        identical(
          data.table::address(result),
          data.table::address(private$.core)
        ) &&
        !identical(data.table::address(result), before_address) &&
        identical(state_of(set)$.values, list(x = 2L)),
        "atomic capsule replacement differs")
    },
    direct_param_set_core_kind = function() {
      set <- ps(x = p_int())
      check(identical(
        .Call(symbol("param_set_core_kind"), private_of(set)),
        1L
      ), "BASE capsule kind differs")
    },
    direct_param_set_shadow_core_new = function() {
      origin <- ps(hidden = p_int(), x = p_dbl(0, 1))
      shadow <- ParamSetShadow$new(origin, "hidden")
      private <- private_of(shadow)
      result <- .Call(
        symbol("param_set_shadow_core_new"),
        private$.core,
        origin
      )
      check(typeof(result) == "externalptr" && identical(
        .Call(symbol("param_set_core_kind"), result),
        3L
      ), "authoritative Shadow capsule construction differs")
    },
    direct_param_set_shadow_construct = function() {
      origin <- ps(hidden = p_int(), x = p_dbl(0, 1))
      result <- .Call(
        symbol("param_set_shadow_construct"),
        origin,
        "hidden"
      )
      state <- .Call(symbol("param_set_core_state"), result)
      check(typeof(result) == "externalptr" && identical(state$.params$id, "x"),
        "native Shadow schema construction differs")
    },
    direct_param_set_shadow_refresh = function() {
      origin <- ps(hidden = p_int(), x = p_dbl(0, 1))
      shadow <- ParamSetShadow$new(origin, "hidden")
      origin$values <- list(hidden = 1L, x = 0.5)
      result <- .Call(
        symbol("param_set_shadow_refresh"),
        shadow,
        private_of(shadow)
      )
      check(typeof(result) == "externalptr" &&
        identical(state_of(shadow)$.values, list(x = 0.5)),
        "authoritative Shadow refresh differs")
    },
    direct_param_set_shadow_constraint = function() {
      origin <- ps(hidden = p_int(), x = p_int())
      origin$values <- list(hidden = 1L)
      origin$constraint <- function(x) x$hidden < x$x
      shadow <- ParamSetShadow$new(origin, "hidden")
      plan <- get("plan", environment(shadow$constraint))
      check(isTRUE(.Call(
        symbol("param_set_shadow_constraint"), plan, list(x = 2L)
      )), "native Shadow constraint adapter differs")
    },
    direct_design_transpose = function() {
      result <- .Call(symbol("design_transpose"), list(x = c(1L, 2L), y = c(3, 4)), FALSE)
      check(identical(result, list(list(x = 1L, y = 3), list(x = 2L, y = 4))),
        "transpose result differs")
    },
    direct_design_transpose_trafos = function() {
      set <- ps(x = p_dbl(trafo = function(value) value * 2))
      result <- .Call(
        symbol("design_transpose_trafos"),
        list(list(x = 0.25)),
        set
      )
      check(identical(result, list(list(x = 0.5))),
        "Design transformation result differs")
    },
    direct_design_dependency_plan = function() {
      set <- ps(parent = p_lgl(), child = p_int(0L, 9L))
      set$add_dep("child", "parent", CondEqual(TRUE))
      data <- data.table::data.table(
        parent = c(TRUE, FALSE),
        child = c(1L, 2L)
      )
      result <- .Call(symbol("design_dependency_plan"), data, set)
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
      check(isTRUE(.Call(
        symbol("domain_check_builtin"), p_int(0L, 2L), list(1L), FALSE
      )), "domain check failed")
    },
    direct_domain_property_builtin = function() {
      result <- .Call(symbol("domain_property_builtin"), p_int(0L, 2L), 0L)
      check(identical(result, 3), "domain property differs")
    },
    direct_domain_construct = function() {
      result <- .Call(symbol("domain_construct"), "ParamDbl", "ParamDbl", NULL,
        0, 1, 0, NULL, list(), get("NO_DEF", namespace), character(), NULL,
        "numeric", FALSE, NULL, 1L, FALSE, "x", NULL)
      check(identical(names(result), get("domain_names", namespace)) &&
        identical(result$cls, "ParamDbl") && identical(result$grouping, "ParamDbl") &&
        identical(result$lower, 0) && identical(result$upper, 1) &&
        identical(result$storage_type, "numeric") && identical(result$.init_given, FALSE),
        "domain constructor result differs")
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
    direct_domain_qunif_builtin = function() {
      check(identical(.Call(symbol("domain_qunif_builtin"), p_dbl(0, 10), c(0, .5, 1)), c(0, 5, 10)), "domain qunif differs")
    },
    direct_domain_sanitize_builtin = function() {
      check(identical(.Call(symbol("domain_sanitize_builtin"), p_dbl(0, 1, tolerance = .1), list(-.05, 1.05)), list(0, 1)), "domain sanitize differs")
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
    direct_param_set_collection_add = function() {
      collection <- psc(left = ps(x = p_int()))
      child <- ps(y = p_lgl())
      result <- .Call(
        symbol("param_set_collection_add"),
        private_of(collection), collection, child, "right", TRUE, TRUE
      )
      check(identical(result, collection) &&
        identical(collection$ids(), c("left.x", "right.y")) &&
        identical(collection$sets[[2L]], child),
        "collection add replacement differs")
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
        length(result$constraint_sets) == 1L &&
        is.function(result$constraint_sets[[1L]]$constraint),
        "collection detachment plan differs")
    },
    direct_param_set_collection_has_callback = function() {
      child <- ps(x = p_int())
      child$extra_trafo <- function(x) x
      collection <- psc(owner = child)
      check(isTRUE(.Call(
        symbol("param_set_collection_has_callback"),
        private_of(collection), collection, 0L
      )) && identical(.Call(
        symbol("param_set_collection_has_callback"),
        private_of(collection), collection, 1L
      ), FALSE), "collection callback feature selection differs")
    },
    direct_param_set_collection_extra_trafo = function() {
      child <- ps(x = p_int(), y = p_int())
      child$extra_trafo <- function(x) list(x = x$x + 1L)
      collection <- psc(owner = child)
      result <- .Call(
        symbol("param_set_collection_extra_trafo"),
        private_of(collection), collection,
        list(unknown = 9L, owner.x = 1L, owner.y = 2L)
      )
      check(identical(result, list(unknown = 9L, owner.x = 2L)),
        "live collection aggregate transformation differs")
    },
    direct_param_set_collection_constraint = function() {
      child <- ps(x = p_int())
      child$constraint <- function(x) x$x < 2L
      collection <- psc(owner = child)
      check(isTRUE(.Call(
        symbol("param_set_collection_constraint"),
        private_of(collection), collection, list(owner.x = 1L)
      )) && identical(.Call(
        symbol("param_set_collection_constraint"),
        private_of(collection), collection, list(owner.x = 2L)
      ), FALSE), "live collection constraint evaluation differs")
    },
    direct_param_set_collection_detached_extra_trafo = function() {
      child <- ps(x = p_int(), y = p_int())
      child$extra_trafo <- function(x) list(x = x$x + 1L)
      collection <- psc(owner = child)
      detached <- collection$subset(collection$ids(),
        allow_dangling_dependencies = TRUE)
      plan <- get("plan", environment(detached$extra_trafo))
      result <- .Call(
        symbol("param_set_collection_detached_extra_trafo"),
        plan, list(unknown = 9L, owner.x = 1L, owner.y = 2L)
      )
      check(identical(result, list(unknown = 9L, owner.x = 2L)),
        "detached collection aggregate transformation differs")
    },
    direct_param_set_collection_detached_constraint = function() {
      child <- ps(x = p_int())
      child$constraint <- function(x) x$x < 2L
      collection <- psc(owner = child)
      detached <- collection$subset("owner.x",
        allow_dangling_dependencies = TRUE)
      plan <- get("plan", environment(detached$constraint))
      check(isTRUE(.Call(
        symbol("param_set_collection_detached_constraint"),
        plan, list(owner.x = 1L)
      )) && identical(.Call(
        symbol("param_set_collection_detached_constraint"),
        plan, list(owner.x = 2L)
      ), FALSE), "detached collection constraint evaluation differs")
    },
    direct_param_set_collection_owner_subset_state = function() {
      child <- ps(x = p_int(), y = p_int())
      token <- .Call(
        symbol("param_set_collection_owner_subset_state"),
        function(x, param_set) x, child, "x"
      )
      owner <- ParamSet$new(token)
      check(identical(owner$ids(), "x") &&
        identical(class(owner), c("ParamSet", "R6")),
        "package-owned collection callback owner subset differs")
    },
    direct_param_set_check_builtin = function() {
      set <- ps(x = p_int(0L, 2L))
      result <- .Call(
        symbol("param_set_check_builtin"),
        private_of(set), set, list(x = 1), TRUE, TRUE, "none", TRUE
      )
      check(isTRUE(result) && identical(attr(result, "sanitized"), list(x = 1L)), "ParamSet scalar check differs")
    },
    direct_tune_token_snapshot_list = function() {
      token <- to_tune(
        lower = c(lower_name = 0),
        upper = c(upper_name = 1),
        logscale = c(scale_name = FALSE)
      )
      set <- ps(x = p_dbl(0, 1))
      result <- .Call(
        symbol("tune_token_snapshot_list"),
        private_of(set),
        set,
        list(x = token)
      )
      check(identical(names(result), c("tokens", "targets")) &&
        identical(names(result$tokens), "x") &&
        identical(class(result$tokens$x), c("RangeTuneToken", "TuneToken")) &&
        is.null(names(result$tokens$x$content$lower)) &&
        is.null(names(result$tokens$x$content$upper)) &&
        is.null(names(result$tokens$x$content$logscale)),
        "closed TuneToken structural snapshot differs")
    },
    direct_param_set_check_dependencies_builtin = function() {
      set <- ps(on = p_lgl(), x = p_int(depends = on == TRUE))
      accepted <- .Call(
        symbol("param_set_check_dependencies_builtin"),
        private_of(set), set, list(on = TRUE, x = 1L)
      )
      rejected <- .Call(
        symbol("param_set_check_dependencies_builtin"),
        private_of(set), set, list(on = FALSE, x = 1L)
      )
      check(isTRUE(accepted) && is.character(rejected) &&
        length(rejected) == 1L,
        "ParamSet dependency-only check differs")
    },
    direct_param_set_test_constraint_builtin = function() {
      set <- ps(x = p_int(0L, 2L))
      set$constraint <- function(x) x$x < 2L
      accepted <- .Call(
        symbol("param_set_test_constraint_builtin"),
        private_of(set), set, list(x = 1L), TRUE
      )
      rejected <- .Call(
        symbol("param_set_test_constraint_builtin"),
        private_of(set), set, list(x = 2L), TRUE
      )
      check(isTRUE(accepted) && identical(rejected, FALSE),
        "ParamSet scalar constraint-only check differs")
    },
    direct_param_set_test_constraint_dt_builtin = function() {
      set <- ps(x = p_int(0L, 2L))
      set$constraint <- function(x) x$x < 2L
      result <- .Call(
        symbol("param_set_test_constraint_dt_builtin"),
        private_of(set), set, plain_table(list(x = c(1L, 2L))), TRUE
      )
      check(identical(result, c(TRUE, FALSE)),
        "ParamSet tabular constraint-only check differs")
    },
    direct_param_set_check_dt_builtin = function() {
      set <- ps(x = p_int(0L, 2L))
      check(isTRUE(.Call(
        symbol("param_set_check_dt_builtin"),
        private_of(set), set, data.frame(x = c(0L, 2L)), TRUE, "all", TRUE
      )), "ParamSet table check differs")
    },
    direct_condition_test_builtin = function() {
      result <- .Call(
        symbol("condition_test_builtin"), CondAnyOf(1:2), 1:3
      )
      check(identical(result, c(TRUE, TRUE, FALSE)),
        "closed built-in Condition vector comparison differs")
    },
    direct_param_set_ids = function() {
      set <- ps(x = p_int(tags = c("red", "fast")), y = p_dbl(tags = "red"))
      state <- state_of(set)
      result <- .Call(symbol("param_set_ids"), state$.params, state$.tags, NULL, c("red", "fast"), NULL)
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
      result <- .Call(symbol("param_set_store_values"), private, set, list(b = 2L, a = 1L))
      check(identical(result, list(a = 1L, b = 2L)), "stored values differ")
    },
    direct_param_set_assign_values_checked = function() {
      set <- ps(x = p_int(0L, 2L))
      result <- .Call(symbol("param_set_assign_values_checked"), private_of(set), set, list(x = 1))
      check(identical(result, list(x = 1L)), "checked assignment differs")
    },
    direct_param_set_set_tags = function() {
      set <- ps(x = p_int(), y = p_lgl())
      result <- .Call(
        symbol("param_set_set_tags"), private_of(set), set,
        list(y = "switch", x = "numeric")
      )
      check(identical(result, list(y = "switch", x = "numeric")) &&
        identical(set$tags, list(x = "numeric", y = "switch")),
        "tag replacement differs")
    },
    direct_param_set_get_tags = function() {
      set <- ps(x = p_int(tags = c("numeric", "required")), y = p_lgl())
      result <- .Call(symbol("param_set_get_tags"), private_of(set), set)
      check(identical(result,
        list(x = c("numeric", "required"), y = character())),
        "tag projection differs")
    },
    direct_param_set_dependency_table_snapshot = function() {
      condition <- CondAnyOf(1:2)
      input <- data.frame(id = "child", on = "parent",
        cond = I(list(condition)))
      result <- .Call(symbol("param_set_dependency_table_snapshot"), input)
      condition$rhs[[1L]] <- 9L
      check(identical(class(result), "data.frame") &&
        identical(result$cond[[1L]]$rhs, 1:2),
        "dependency table snapshot differs")
    },
    direct_param_set_dependencies = function() {
      set <- ps(parent = p_lgl(), child = p_int(depends = parent == TRUE))
      result <- .Call(symbol("param_set_dependencies"), private_of(set), set)
      check(identical(result$id, "child") && identical(result$on, "parent"),
        "dependency projection differs")
    },
    direct_param_set_has_dependencies = function() {
      child <- ps(parent = p_lgl(), child = p_int())
      collection <- ParamSetCollection$new(list(owner = child))
      shadow <- ParamSetShadow$new(child, character())
      before <- c(
        base = .Call(
          symbol("param_set_has_dependencies"), private_of(child), child
        ),
        collection = .Call(
          symbol("param_set_has_dependencies"),
          private_of(collection), collection
        ),
        shadow = .Call(
          symbol("param_set_has_dependencies"), private_of(shadow), shadow
        )
      )
      child$add_dep("child", "parent", CondEqual(TRUE))
      after <- c(
        base = .Call(
          symbol("param_set_has_dependencies"), private_of(child), child
        ),
        collection = .Call(
          symbol("param_set_has_dependencies"),
          private_of(collection), collection
        ),
        shadow = .Call(
          symbol("param_set_has_dependencies"), private_of(shadow), shadow
        )
      )
      check(
        identical(before, c(
          base = FALSE, collection = FALSE, shadow = FALSE
        )) && identical(after, c(
          base = TRUE, collection = TRUE, shadow = TRUE
        )),
        "scalar dependency presence differs"
      )
    },
    direct_param_set_set_dependencies = function() {
      set <- ps(parent = p_int(0L, 2L), child = p_lgl())
      input <- data.frame(id = "child", on = "parent",
        cond = I(list(CondEqual(1L))))
      result <- .Call(
        symbol("param_set_set_dependencies"), private_of(set), set, input
      )
      check(identical(result, input) && identical(set$deps$on, "parent"),
        "dependency replacement differs")
    },
    direct_param_set_add_dependency = function() {
      set <- ps(parent = p_int(0L, 2L), child = p_lgl())
      result <- .Call(
        symbol("param_set_add_dependency"), private_of(set), set,
        "child", "parent", CondEqual(1L), FALSE
      )
      check(identical(result, set) && identical(set$deps$id, "child"),
        "dependency append differs")
    },
    direct_param_set_set_callback = function() {
      set <- ps(x = p_int())
      callback <- function(x) FALSE
      result <- .Call(
        symbol("param_set_set_callback"), private_of(set), set,
        callback, 1L
      )
      check(identical(result, callback) && identical(set$constraint, callback),
        "callback replacement differs")
    },
    direct_param_set_property = function() {
      set <- ps(x = p_int(0L, 2L))
      result <- .Call(symbol("param_set_property"), state_of(set)$.params, 0L)
      check(identical(result, c(x = 3)), "property vector differs")
    },
    direct_param_set_qunif_builtin = function() {
      set <- ps(x = p_int(0L, 10L), y = p_dbl(0, 1))
      private <- private_of(set)
      units <- matrix(c(.5, .25), nrow = 1L, dimnames = list(NULL, c("x", "y")))
      result <- .Call(
        symbol("param_set_qunif_builtin"), private, set, units
      )
      check(identical(result$x, 5L) && identical(result$y, .25), "ParamSet qunif differs")
      malformed <- tryCatch(
        .Call(
          symbol("param_set_qunif_builtin"),
          private,
          set,
          matrix(NA_real_, nrow = 1L, dimnames = list(NULL, "x"))
        ),
        error = identity
      )
      check(
        inherits(malformed, "error") &&
          grepl("must not be missing", conditionMessage(malformed), fixed = TRUE),
        "malformed ParamSet qunif input did not error deterministically"
      )
    },
    direct_sampler_unif_sample_builtin = function() {
      set <- ps(
        x = p_int(0L, 10L),
        y = p_dbl(0, 1),
        z = p_fct(c("left", "right")),
        flag = p_lgl()
      )
      set.seed(1729L)
      result <- .Call(
        symbol("sampler_unif_sample_builtin"),
        set,
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
        state_of(set)$.params,
        c(y = 3, x = 3)
      )
      check(
        identical(names(result), c("y", "x")) &&
          identical(result$y, rep(c(0, .5, 1), each = 3L)) &&
          identical(result$x, rep(0:2, times = 3L)) &&
          data.table:::selfrefok(result, verbose = FALSE) == 1L,
        "one-shot grid differs"
      )
      overflow <- tryCatch(
        .Call(
          symbol("generate_design_grid_builtin"),
          state_of(set)$.params,
          c(y = 50000, x = 50000)
        ),
        error = identity
      )
      check(
        inherits(overflow, "error") &&
          grepl("Grid product exceeds", conditionMessage(overflow), fixed = TRUE),
        "overflowing native grid did not error before allocation"
      )
    },
    direct_param_set_trafo = function() {
      set <- ps(x = p_dbl(0, 1, trafo = function(value) value * 2))
      result <- .Call(
        symbol("param_set_trafo"),
        private_of(set), set, list(x = 0.25), set
      )
      check(identical(result, list(x = 0.5)), "ParamSet trafo differs")
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
      result <- .Call(symbol("param_set_collection_values"), private, collection)
      check(identical(result, list(owner.x = 1L)), "collection values differ")
    },
    direct_param_set_subset_state = function() {
      set <- ps(x = p_int(), y = p_dbl(trafo = exp))
      set$extra_trafo <- function(x, param_set) x
      token <- .Call(
        symbol("param_set_subset_state"),
        private_of(set), set, "y", FALSE, TRUE,
        set$constraint, set$extra_trafo, FALSE
      )
      target <- subset_private()
      adopted <- .Call(symbol("param_set_adopt_subset_state"), target, token)
      result <- .Call(symbol("param_set_core_state"), target)
      check(isTRUE(adopted) && nrow(result$.trafos) == 0L &&
        is.null(result$.extra_trafo), "stripped subset capsule transaction differs")
    },
    direct_param_set_subspace_states = function() {
      set <- ps(x = p_int(init = 1L), y = p_lgl(init = TRUE))
      result <- .Call(symbol("param_set_subspace_states"), private_of(set),
        set, c("x", "y"), set$extra_trafo)
      check(identical(names(result), c("x", "y")) &&
        all(vapply(result, function(core) {
          typeof(core) == "externalptr"
        }, logical(1L))), "bulk subspace capsule transactions differ")
    },
    direct_param_set_adopt_subset_state = function() {
      set <- ps(x = p_int(), y = p_lgl())
      plan <- .Call(
        symbol("param_set_subset_state"),
        private_of(set), set, "y", FALSE, TRUE,
        set$constraint, set$extra_trafo, TRUE
      )
      target <- subset_private()
      check(isTRUE(.Call(symbol("param_set_adopt_subset_state"), target, plan)),
        "subset adoption failed")
      adopted <- .Call(symbol("param_set_core_state"), target)
      check(identical(adopted$.params$id, "y"), "adopted subset differs")
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
    direct_test_stateful_altrep_row_names_rearm = function() {
      callbacks <- 0L
      outer <- NULL
      row_names <- stateful(
        c("row-a", "row-b"),
        c("later-a", "later-b"),
        callback = function() {
          callbacks <<- callbacks + 1L
          data.table::setnames(outer, c("mutated_y", "mutated_x"))
          invisible(gc())
        }
      )
      frame <- structure(
        list(x = c(1L, 2L), y = c(3L, 4L)),
        names = c("x", "y"),
        row.names = row_names,
        class = "data.frame"
      )
      outer <- frame
      return_value <- .Call(
        symbol("test_stateful_altrep_row_names_rearm"),
        outer,
        c(NA_integer_, 0L)
      )
      rows <- .Call(symbol("design_transpose"), outer, FALSE)
      check(is.null(return_value) && identical(callbacks, 1L) &&
        identical(names(outer), c("mutated_y", "mutated_x")) &&
        identical(names(rows[[1L]]), c("x", "y")),
        "row-name Length reentry escaped owned table metadata")
    },
    direct_test_public_row_names_count = function() {
      callbacks <- 0L
      value <- stateful(
        c("row-a", "row-b"),
        c("later-a", "later-b", "later-c"),
        length_switch_after = 1L,
        callback = function() {
          callbacks <<- callbacks + 1L
          invisible(gc())
        },
        callback_after = c(0L, 0L)
      )
      result <- .Call(symbol("test_public_row_names_count"), value)
      check(identical(result, 2) && identical(callbacks, 1L),
        "public row-name metadata was replayed or inspected by label")
    },
    direct_test_materialize_public_table_shell = function() {
      table <- data.table::data.table(x = c(2L, 1L), y = c(1L, 2L))
      data.table::setkeyv(table, "x")
      data.table::setindexv(table, "y")
      outer <- stateful(table, table)
      result <- .Call(symbol("test_materialize_public_table_shell"), outer)
      check(identical(names(result), c("x", "y")) &&
        identical(class(result), c("data.table", "data.frame")) &&
        is.null(attr(result, ".internal.selfref", exact = TRUE)) &&
        is.null(attr(result, "sorted", exact = TRUE)) &&
        is.null(attr(result, "index", exact = TRUE)) &&
        identical(data.table::address(result[[1L]]),
          data.table::address(table[[1L]])) &&
        identical(data.table::address(result[[2L]]),
          data.table::address(table[[2L]])),
        "public-table materialization retained caches or changed columns")
    },
    direct_test_gc_column_mutator = function() {
      table <- list(x = 1L)
      pointer <- .Call(symbol("test_gc_column_mutator"), table, 0L, 2L)
      check(typeof(pointer) == "externalptr", "GC mutator did not return an external pointer")
    },
    direct_test_tune_token_gc_mutation_snapshot = function() {
      token <- to_tune(0, 1)
      result <- .Call(
        symbol("test_tune_token_gc_mutation_snapshot"),
        token,
        0L,
        0.25
      )
      observed <- result$content$lower
      check(inherits(result, "RangeTuneToken") && identical(observed, 0.25),
        "TuneToken GC-mutation snapshot fixture differs")
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
      result <- .Call(
        symbol("domain_check_builtin"), domain, list(value), FALSE
      )
      check(is.character(result) && length(result) == 1L &&
        !is.na(result) && grepl("within the Domain bounds", result, fixed = TRUE),
        "callback-rooted Domain snapshot differs")
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
