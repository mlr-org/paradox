# Deterministic characterization cases for the public and widely observed
# paradox surface.  Each case returns observations, never an R6 object itself.

diff_case <- function(description, run, seed = 1L) {
  list(description = description, run = run, seed = as.integer(seed))
}

observe_call <- function(expr) {
  warnings <- character()
  value <- tryCatch(
    withCallingHandlers(
      list(status = "value", value = force(expr)),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) list(
      status = "error",
      class = class(e),
      message = conditionMessage(e),
      call = if (is.null(conditionCall(e))) NULL else paste(deparse(conditionCall(e)), collapse = "\n")
    )
  )
  c(value, list(warnings = warnings))
}

project_domain <- function(domain) {
  list(
    class = class(domain),
    columns = names(domain),
    column_types = vapply(domain, typeof, character(1L)),
    lower = domain$lower,
    upper = domain$upper,
    tolerance = domain$tolerance,
    levels = domain$levels,
    special_vals = domain$special_vals,
    default = domain$default,
    storage_type = domain$storage_type,
    tags = domain$.tags,
    init_given = domain$.init_given,
    init = domain$.init,
    representation = if (is.null(attr(domain, "repr"))) NULL else paste(deparse(attr(domain, "repr")), collapse = "\n")
  )
}

paradox_differential_cases <- list(
  shape = diff_case(
    "Domain constructors and the stable ParamSet property/table shape",
    function() {
      domains <- list(
        ratio = paradox::p_dbl(-2.5, 4.5, default = 0, special_vals = list(Inf), tags = c("model", "numeric")),
        count = paradox::p_int(0, 7, default = 3L, tags = "model"),
        method = paradox::p_fct(c("small", "large"), default = "small"),
        enabled = paradox::p_lgl(default = TRUE),
        payload = paradox::p_uty(default = list(code = 1L))
      )
      parameter_set <- do.call(paradox::ps, domains)
      list(
        domains = lapply(domains, project_domain),
        class = class(parameter_set),
        ids = parameter_set$ids(),
        ids_by_class = parameter_set$ids(class = c("ParamDbl", "ParamInt")),
        ids_by_tag = parameter_set$ids(tags = "model"),
        flags = list(
          length = parameter_set$length,
          is_empty = parameter_set$is_empty,
          has_trafo = parameter_set$has_trafo,
          has_deps = parameter_set$has_deps,
          all_numeric = parameter_set$all_numeric,
          all_categorical = parameter_set$all_categorical,
          all_bounded = parameter_set$all_bounded
        ),
        properties = list(
          class = parameter_set$class,
          lower = parameter_set$lower,
          upper = parameter_set$upper,
          levels = parameter_set$levels,
          nlevels = parameter_set$nlevels,
          is_number = parameter_set$is_number,
          is_categ = parameter_set$is_categ,
          is_bounded = parameter_set$is_bounded,
          storage_type = parameter_set$storage_type,
          special_vals = parameter_set$special_vals,
          default = parameter_set$default,
          tags = parameter_set$tags
        ),
        data = parameter_set$data,
        format = parameter_set$format()
      )
    }
  ),

  domain_construction = diff_case(
    "Built-in Domain construction, NSE representation, callbacks, initialization, and extension fallback",
    function() {
      lower <- 2L
      upper <- 8L
      requirement <- quote(parent == 3L)
      offset <- 7L
      callback <- function(value) {
        if (is.list(value)) {
          value$offset <- offset
          value
        } else {
          value + offset
        }
      }
      aggregate <- function(values) sum(unlist(values))
      in_tune <- function(domain, param_vals) domain$upper
      disable <- list(early_stopping = NULL)
      custom_check <- function(value) {
        if (is.list(value)) TRUE else "must be a list"
      }

      domains <- list(
        dbl = paradox::p_dbl(lower, upper = upper, default = 3, tags = c("numeric", "bounded")),
        dbl_log = paradox::p_dbl(1, 10, logscale = TRUE),
        int = paradox::p_int(-2L, 5L, tolerance = 0, init = 1L),
        int_log = paradox::p_int(0, 20, logscale = TRUE),
        fct = paradox::p_fct(
          list(one = 1L, two = list(value = 2L)),
          depends = requirement,
          trafo = callback
        ),
        lgl = paradox::p_lgl(default = TRUE, special_vals = list("automatic")),
        uty = paradox::p_uty(
          custom_check = custom_check,
          default = list(code = 1L),
          repr = quote(payload),
          tags = "payload"
        ),
        internal = paradox::p_int(
          1L,
          10L,
          tags = "internal_tuning",
          aggr = aggregate,
          in_tune_fn = in_tune,
          disable_in_tune = disable
        )
      )

      observe_domain <- function(domain) {
        c(project_domain(domain), list(
          id = domain$id,
          dimensions = dim(domain),
          attribute_names = names(attributes(domain)),
          cargo_names = names(domain$cargo[[1L]]),
          printed = paste(capture.output(print(domain)), collapse = "\n")
        ))
      }

      factor_requirement <- domains$fct$.requirements[[1L]][[1L]]
      p_custom <- function(lower = 0, upper = 1, init) {
        paradox:::Domain(
          cls = "ParamCustom",
          grouping = "ParamCustom",
          lower = lower,
          upper = upper,
          tolerance = 0,
          storage_type = "numeric",
          init = init
        )
      }
      custom <- p_custom(-1, 2)

      list(
        domains = lapply(domains, observe_domain),
        transformations = list(
          dbl_log = domains$dbl_log$.trafo[[1L]](log(2)),
          int_log = domains$int_log$.trafo[[1L]](log(3)),
          factor_one = domains$fct$.trafo[[1L]]("one"),
          factor_two = domains$fct$.trafo[[1L]]("two")
        ),
        dependency = list(
          on = factor_requirement$on,
          class = class(factor_requirement$cond),
          text = paradox::condition_as_string(factor_requirement$cond, factor_requirement$on),
          probes = paradox::condition_test(factor_requirement$cond, c(2L, 3L, 4L))
        ),
        callbacks = list(
          aggregate = domains$internal$cargo[[1L]]$aggr(list(1L, 2L, 3L)),
          in_tune = domains$internal$cargo[[1L]]$in_tune_fn(domains$internal, list()),
          disable = domains$internal$cargo[[1L]]$disable_in_tune,
          custom_valid = domains$uty$cargo[[1L]]$custom_check(list()),
          custom_invalid = domains$uty$cargo[[1L]]$custom_check(1L)
        ),
        extension = observe_domain(custom),
        extension_roundtrip = identical(
          unserialize(serialize(custom, NULL, version = 3L)),
          custom
        ),
        diagnostics = list(
          duplicate_tags = observe_call(paradox::p_lgl(tags = c("x", "x"))),
          special_and_trafo = observe_call(paradox::p_lgl(
            special_vals = list("automatic"),
            trafo = identity
          )),
          invalid_default = observe_call(paradox::p_lgl(default = 1L)),
          invalid_init = observe_call(paradox::p_lgl(init = 1L))
        )
      )
    },
    seed = 31L
  ),

  domain_lazy_arguments = diff_case(
    "Domain validation and representation preserve opaque promise forcing order",
    function() {
      events <- new.env(parent = emptyenv())
      events$seen <- character()
      invalid_tags <- observe_call(paradox::p_lgl(
        tags = 1,
        default = {
          events$seen <- c(events$seen, "default")
          FALSE
        },
        init = {
          events$seen <- c(events$seen, "init")
          TRUE
        }
      ))
      invalid_tag_events <- events$seen

      events$seen <- character()
      delayedAssign(
        "invalid_dependency",
        {
          events$seen <- c(events$seen, "depends")
          1L
        },
        assign.env = environment(),
        eval.env = environment()
      )
      invalid_depends <- observe_call(paradox::p_lgl(
        depends = invalid_dependency,
        init = {
          events$seen <- c(events$seen, "init")
          TRUE
        }
      ))
      invalid_dependency_events <- events$seen

      events$seen <- character()
      valid <- paradox::p_lgl(
        default = {
          events$seen <- c(events$seen, "default")
          FALSE
        },
        init = {
          events$seen <- c(events$seen, "init")
          TRUE
        }
      )
      list(
        invalid_tags = invalid_tags,
        invalid_tag_events = invalid_tag_events,
        invalid_depends = invalid_depends,
        invalid_dependency_events = invalid_dependency_events,
        valid_events = events$seen,
        valid_default = valid$default,
        valid_init_given = valid$.init_given,
        valid_init = valid$.init
      )
    },
    seed = 41L
  ),

  ids_filter_edges = diff_case(
    "Empty, overlapping, and order-sensitive ParamSet ID filters",
    function() {
      parameter_set <- paradox::ps(
        zebra = paradox::p_int(tags = c("red", "blue")),
        alpha = paradox::p_int(tags = "blue"),
        middle = paradox::p_int(tags = c("red", "green")),
        omega = paradox::p_int()
      )

      tags_empty <- parameter_set$ids(tags = character())
      any_overlap <- parameter_set$ids(any_tags = c("blue", "red"))
      any_multiple <- parameter_set$ids(any_tags = c("blue", "green"))

      list(
        unfiltered = parameter_set$ids(),
        tags_empty = tags_empty,
        tags_empty_type = typeof(tags_empty),
        tags_empty_length = length(tags_empty),
        any_overlap = any_overlap,
        any_overlap_duplicated = anyDuplicated(any_overlap),
        any_overlap_in_original_order = identical(any_overlap, c("zebra", "alpha", "middle")),
        any_multiple = any_multiple,
        any_multiple_in_original_order = identical(any_multiple, c("zebra", "alpha", "middle")),
        all_tags = parameter_set$ids(tags = c("red", "blue")),
        combined = parameter_set$ids(tags = "red", any_tags = c("blue", "green"))
      )
    },
    seed = 51L
  ),

  subset_cartesian = diff_case(
    "Subsetting repeated IDs beyond data.table's historical Cartesian join limit",
    function() {
      parameter_set <- paradox::ps(
        repeated = paradox::p_int(
          0,
          10,
          tags = c("first", "second", "third"),
          trafo = identity
        )
      )
      parameter_set$values <- list(repeated = 4L)
      requested <- rep("repeated", 5L)

      observe_call({
        result <- parameter_set$subset(
          requested,
          allow_dangling_dependencies = TRUE
        )
        private <- result$.__enclos_env__$private
        list(
          class = class(result),
          ids = result$ids(),
          values = result$values,
          params = private$.params,
          tags = private$.tags,
          trafos = list(
            ids = private$.trafos$id,
            count = nrow(private$.trafos),
            all_identity = all(vapply(
              private$.trafos$trafo,
              identical,
              logical(1L),
              identity
            ))
          ),
          deps = private$.deps,
          assert_values = result$assert_values
        )
      })
    },
    seed = 61L
  ),

  validation = diff_case(
    "Scalar, tabular, sanitizing, and quantile validation paths",
    function() {
      parameter_set <- paradox::ps(
        ratio = paradox::p_dbl(0, 1, tolerance = 1e-8),
        count = paradox::p_int(1, 4),
        method = paradox::p_fct(c("a", "b")),
        enabled = paradox::p_lgl(),
        payload = paradox::p_uty(custom_check = function(x) if (is.raw(x)) TRUE else "must be raw")
      )
      rows <- data.table::data.table(
        ratio = c(0, 0.5, 1),
        count = c(1L, 3L, 4L),
        method = c("a", "b", "a"),
        enabled = c(TRUE, FALSE, TRUE),
        payload = list(as.raw(1), as.raw(2), as.raw(3))
      )
      integer_domain <- paradox::p_int(-2, 2, tolerance = 1e-6)
      double_domain <- paradox::p_dbl(-1, 1, tolerance = 1e-6)
      list(
        checks = list(
          valid = parameter_set$check(as.list(rows[1L])),
          wrong_bound = parameter_set$check(list(ratio = 1.5)),
          wrong_type = parameter_set$check(list(count = "3")),
          unknown = parameter_set$check(list(unknown = 1), check_strict = TRUE),
          non_strict_unknown = parameter_set$check(list(unknown = 1), check_strict = FALSE),
          table = parameter_set$check_dt(rows),
          bad_table = parameter_set$check_dt(data.table::copy(rows)[2L, count := 9L])
        ),
        assertions = list(
          sanitized_integer = observe_call(parameter_set$assert(list(count = 2.0000001), sanitize = TRUE)),
          invalid_payload = observe_call(parameter_set$assert(list(payload = "not raw")))
        ),
        domains = list(
          integer_check = paradox::domain_check(integer_domain, list(-2L)),
          integer_sanitize = paradox::domain_sanitize(integer_domain, list(-1.9999999, 1.9999999)),
          integer_qunif = paradox::domain_qunif(integer_domain, c(0, 0.2, 0.5, 0.8, 1)),
          double_check = paradox::domain_check(double_domain, list(1 + 5e-7)),
          double_sanitize = paradox::domain_sanitize(double_domain, list(-1 - 5e-7, 1 + 5e-7)),
          double_qunif = paradox::domain_qunif(double_domain, c(0, 0.25, 0.5, 0.75, 1))
        )
      )
    },
    seed = 101L
  ),

  grouped_sanitization = diff_case(
    "Per-parameter bounds during grouped numeric sanitization",
    function() {
      parameter_set <- paradox::ps(
        first = paradox::p_dbl(0, 1, tolerance = 0.1),
        second = paradox::p_dbl(-2, 2, tolerance = 0.2),
        third = paradox::p_dbl(10, 20, tolerance = 0.1)
      )
      settings <- list(first = -0.05, second = 2.3, third = 20.5)

      list(
        settings = settings,
        sanitized_check = observe_call(parameter_set$check(settings, sanitize = TRUE))
      )
    },
    seed = 111L
  ),

  infinite_bounds = diff_case(
    "One-sided and fixed infinite double bounds at zero/default tolerance",
    function() {
      cases <- list(
        lower_finite = list(
          domain = paradox::p_dbl(0, Inf, tolerance = 0),
          valid = list(0, 1, Inf),
          invalid = -1
        ),
        upper_finite = list(
          domain = paradox::p_dbl(-Inf, 1, tolerance = 0),
          valid = list(-Inf, 0, 1),
          invalid = 2
        ),
        positive_point = list(
          domain = paradox::p_dbl(Inf, Inf, tolerance = 0),
          valid = list(Inf),
          invalid = -Inf
        ),
        negative_point = list(
          domain = paradox::p_dbl(-Inf, -Inf, tolerance = 0),
          valid = list(-Inf),
          invalid = Inf
        ),
        positive_point_default = list(
          domain = paradox::p_dbl(Inf, Inf),
          valid = list(Inf),
          invalid = -Inf
        ),
        negative_point_default = list(
          domain = paradox::p_dbl(-Inf, -Inf),
          valid = list(-Inf),
          invalid = Inf
        ),
        two_sided = list(
          domain = paradox::p_dbl(-Inf, Inf),
          valid = list(-Inf, 0, Inf),
          invalid = NA_real_
        )
      )

      observe_case <- function(case) {
        parameter_set <- paradox::ParamSet$new(list(x = case$domain))
        units <- c(0, 0.5, 1)
        unit_matrix <- matrix(
          units,
          ncol = 1L,
          dimnames = list(NULL, "x")
        )
        list(
          domain_valid = lapply(
            case$valid,
            function(value) observe_call(paradox::domain_check(case$domain, list(value)))
          ),
          scalar_valid = lapply(
            case$valid,
            function(value) observe_call(parameter_set$check(list(x = value)))
          ),
          table_valid = lapply(
            case$valid,
            function(value) observe_call(parameter_set$check_dt(data.frame(x = value)))
          ),
          domain_invalid = observe_call(paradox::domain_test(case$domain, list(case$invalid))),
          scalar_invalid = observe_call(parameter_set$test(list(x = case$invalid))),
          table_invalid = observe_call(parameter_set$test_dt(data.frame(x = case$invalid))),
          domain_quantiles = observe_call(paradox::domain_qunif(case$domain, units)),
          table_quantiles = observe_call(parameter_set$qunif(unit_matrix))
        )
      }

      lapply(cases, observe_case)
    },
    seed = 121L
  ),

  boundary_matrix = diff_case(
    "Adversarial missing, non-finite, tolerance, special, empty, and name-order inputs",
    function() {
      double_domain <- paradox::p_dbl(
        -1,
        1,
        tolerance = 1e-8,
        special_vals = list(NA_real_, NaN, Inf, -Inf, "AUTO")
      )
      integer_domain <- paradox::p_int(
        -2,
        2,
        tolerance = 1e-6,
        special_vals = list(NA_integer_, Inf, "AUTO")
      )
      factor_domain <- paradox::p_fct(
        c("a", "b"),
        special_vals = list(NA_character_, "AUTO")
      )
      logical_domain <- paradox::p_lgl(special_vals = list(NA, "AUTO"))

      parameter_set <- paradox::ps(
        d = double_domain,
        i = integer_domain,
        f = factor_domain,
        l = logical_domain
      )

      double_values <- list(
        lower = -1,
        just_inside_lower_tolerance = -1 - 0.5e-8,
        just_outside_lower_tolerance = -1 - 2e-8,
        upper = 1,
        just_inside_upper_tolerance = 1 + 0.5e-8,
        just_outside_upper_tolerance = 1 + 2e-8,
        missing = NA_real_,
        nan = NaN,
        positive_infinity = Inf,
        negative_infinity = -Inf,
        special_string = "AUTO",
        wrong_typed_missing = NA_integer_,
        empty_double = numeric()
      )
      integer_values <- list(
        lower = -2L,
        upper = 2L,
        near_integer = 1 + 0.5e-6,
        outside_tolerance = 1 + 2e-6,
        missing = NA_integer_,
        positive_infinity = Inf,
        special_string = "AUTO",
        wrong_typed_missing = NA_real_,
        empty_integer = integer()
      )

      check_each <- function(domain, values) {
        lapply(values, function(value) paradox::domain_check(domain, list(value)))
      }

      valid_forward <- list(d = 0.25, i = 1L, f = "b", l = TRUE)
      valid_reverse <- valid_forward[rev(names(valid_forward))]
      empty_table <- data.table::data.table(
        d = numeric(),
        i = integer(),
        f = character(),
        l = logical()
      )

      list(
        double_checks = check_each(double_domain, double_values),
        integer_checks = check_each(integer_domain, integer_values),
        factor_checks = check_each(factor_domain, list(
          valid = "a",
          invalid = "c",
          missing = NA_character_,
          special = "AUTO",
          factor_value = factor("a", levels = c("a", "b")),
          empty = character()
        )),
        logical_checks = check_each(logical_domain, list(
          true = TRUE,
          false = FALSE,
          missing = NA,
          special = "AUTO",
          integer_one = 1L,
          empty = logical()
        )),
        parameter_set_checks = list(
          forward = parameter_set$check(valid_forward),
          reverse = parameter_set$check(valid_reverse),
          unnamed = parameter_set$check(unname(valid_forward)),
          empty_none = parameter_set$check(list(), presence = "none"),
          empty_all = parameter_set$check(list(), presence = "all"),
          mixed_specials = parameter_set$check(list(d = NaN, i = "AUTO", f = NA_character_, l = "AUTO")),
          typed_missing = parameter_set$check(list(d = NA_real_, i = NA_integer_, f = NA_character_, l = NA)),
          extra_reordered = parameter_set$check(c(list(extra = 1L), valid_reverse), check_strict = TRUE),
          empty_table = parameter_set$check_dt(empty_table),
          empty_table_all = parameter_set$check_dt(empty_table, presence = "all")
        ),
        sanitization = list(
          double = paradox::domain_sanitize(double_domain, list(-1 - 0.5e-8, 1 + 0.5e-8)),
          integer = paradox::domain_sanitize(integer_domain, list(-1.9999999, 1.9999999))
        )
      )
    },
    seed = 151L
  ),

  dependencies = diff_case(
    "Dependency construction, formatting, activation, and design masking",
    function() {
      parameter_set <- paradox::ps(
        mode = paradox::p_fct(c("small", "wide", "deep")),
        width = paradox::p_int(1, 8, depends = mode == "wide"),
        depth = paradox::p_int(1, 4, depends = mode %in% c("wide", "deep"))
      )
      grid <- paradox::generate_design_grid(parameter_set, resolution = 2L)
      list(
        deps = lapply(seq_len(nrow(parameter_set$deps)), function(i) {
          row <- parameter_set$deps[i]
          list(
            id = row$id,
            on = row$on,
            class = class(row$cond[[1L]]),
            text = paradox::condition_as_string(row$cond[[1L]], row$on),
            probes = paradox::condition_test(row$cond[[1L]], c("small", "wide", "deep", NA_character_))
          )
        }),
        checks = list(
          small = parameter_set$check(list(mode = "small")),
          small_with_width = parameter_set$check(list(mode = "small", width = 2L)),
          wide_complete = parameter_set$check(list(mode = "wide", width = 2L, depth = 3L)),
          dependencies_only = parameter_set$check_dependencies(list(mode = "deep", depth = 2L))
        ),
        design = grid$data,
        transposed = grid$transpose(filter_na = TRUE, trafo = FALSE)
      )
    },
    seed = 202L
  ),

  transformations = diff_case(
    "Individual, implicit categorical, extra transformations, and closure capture",
    function() {
      offset <- 3
      callback <- function(value) value + offset
      parameter_set <- paradox::ps(
        x = paradox::p_dbl(0, 1, trafo = callback),
        choice = paradox::p_fct(list(one = 1L, two = 2L)),
        .extra_trafo = function(x, param_set) {
          x$total <- x$x + x$choice + param_set$length
          x
        }
      )
      edge_parameter_set <- paradox::ps(
        nothing = paradox::p_uty(trafo = function(value) NULL),
        many = paradox::p_int(trafo = function(value) {
          c(value, value + 10L)
        })
      )
      vector_parameter_set <- paradox::ps(
        x = paradox::p_dbl(trafo = callback)
      )
      list(
        callback = callback,
        callback_probes = callback(c(-1, 0, 2)),
        transformed = parameter_set$trafo(list(x = 0.5, choice = "two")),
        partial = parameter_set$trafo(list(x = 0.25)),
        reordered_unknown = parameter_set$trafo(list(
          choice = "one", unknown = 7L, x = 0.75
        )),
        null_and_vector = edge_parameter_set$trafo(list(
          many = 2L, nothing = "payload"
        )),
        data_frame_columns = vector_parameter_set$trafo(data.frame(
          x = c(0.1, 0.2)
        )),
        has_trafo_param = parameter_set$has_trafo_param,
        has_extra_trafo = parameter_set$has_extra_trafo
      )
    },
    seed = 303L
  ),

  values_and_serialization = diff_case(
    "Value ordering, unset-via-NULL, clone behavior, and R serialization",
    function() {
      parameter_set <- paradox::ps(
        x = paradox::p_int(0, 10),
        y = paradox::p_dbl(-1, 1),
        z = paradox::p_fct(c("a", "b"))
      )
      parameter_set$set_values(z = "b", x = 2L)
      initial <- parameter_set$values
      parameter_set$set_values(x = NULL, y = 0.5)
      after_unset <- parameter_set$values

      shallow <- parameter_set$clone(deep = FALSE)
      deep <- parameter_set$clone(deep = TRUE)
      restored <- unserialize(serialize(parameter_set, NULL, version = 3L))
      parameter_set$set_values(z = "a")

      list(
        initial = initial,
        after_unset = after_unset,
        current = parameter_set$values,
        shallow = shallow$values,
        deep = deep$values,
        restored = restored$values,
        restored_class = class(restored),
        restored_check = restored$check(restored$values),
        subset = parameter_set$subset(c("z", "y"))$data
      )
    },
    seed = 404L
  ),

  presence = diff_case(
    "Required/all presence checks, including empty settings",
    function() {
      parameter_set <- paradox::ps(
        required = paradox::p_int(0, 5, tags = "required"),
        optional = paradox::p_dbl(0, 1)
      )
      list(
        empty_none = parameter_set$check(list(), presence = "none"),
        empty_required = parameter_set$check(list(), presence = "required"),
        empty_all = parameter_set$check(list(), presence = "all"),
        required_only = parameter_set$check(list(required = 1L), presence = "required"),
        required_only_all = parameter_set$check(list(required = 1L), presence = "all")
      )
    },
    seed = 505L
  ),

  collection_callbacks = diff_case(
    "ParamSetCollection callback inputs, exactly-once transformations, and live child constraints",
    function() {
      transformation_calls <- 0L
      constraint_inputs <- list()
      left <- paradox::ps(
        x = paradox::p_int(0, 10),
        .extra_trafo = function(x) {
          transformation_calls <<- transformation_calls + 1L
          x$x <- x$x + 1L
          x
        }
      )
      right <- paradox::ps(
        y = paradox::p_int(0, 10),
        .constraint = function(x) {
          constraint_inputs[[length(constraint_inputs) + 1L]] <<- x
          TRUE
        }
      )
      collection <- paradox::psc(left = left, right = right)
      collection$values <- list(left.x = 2L, right.y = 3L)
      transformed <- collection$trafo(list(left.x = 2L, right.y = 3L))
      constraint_result <- collection$constraint(list(left.x = 2L, right.y = 3L))

      live_constraint_inputs <- list()
      right$constraint <- function(x) {
        live_constraint_inputs[[length(live_constraint_inputs) + 1L]] <<- x
        value <- if (!is.null(x$y)) x$y else x[["right.y"]]
        value <= 4L
      }
      invalid <- list(left.x = 2L, right.y = 8L)
      valid <- list(left.x = 2L, right.y = 3L)
      live_constraint_result <- collection$constraint(invalid)
      live_test_constraint_invalid <- collection$test_constraint(invalid)
      live_check_invalid <- collection$check(invalid)
      live_test_constraint_valid <- collection$test_constraint(valid)
      live_check_valid <- collection$check(valid)

      flattened <- collection$flatten()
      flattened_constraint_result <- flattened$test_constraint(valid)
      list(
        class = class(collection),
        ids = collection$ids(),
        values = collection$values,
        transformed = transformed,
        transformation_calls = transformation_calls,
        constraint_result = constraint_result,
        live_constraint_result = live_constraint_result,
        live_test_constraint_invalid = live_test_constraint_invalid,
        live_check_invalid = live_check_invalid,
        live_test_constraint_valid = live_test_constraint_valid,
        live_check_valid = live_check_valid,
        flattened_constraint_result = flattened_constraint_result,
        constraint_inputs = constraint_inputs,
        live_constraint_inputs = live_constraint_inputs,
        data = collection$data,
        flattened_class = class(flattened),
        flattened_values = flattened$values
      )
    },
    seed = 606L
  ),

  designs = diff_case(
    "Deterministic grid/random design generation and transposition",
    function() {
      parameter_set <- paradox::ps(
        x = paradox::p_dbl(1, 100, logscale = TRUE),
        n = paradox::p_int(0, 3),
        label = paradox::p_fct(c("a", "b"))
      )
      grid <- paradox::generate_design_grid(parameter_set, resolution = 3L)
      random <- paradox::generate_design_random(parameter_set, 5L)
      list(
        grid_data = grid$data,
        grid_plain = grid$transpose(trafo = FALSE),
        grid_transformed = grid$transpose(trafo = TRUE),
        random_data = random$data,
        random_plain = random$transpose(trafo = FALSE),
        random_transformed = random$transpose(trafo = TRUE)
      )
    },
    seed = 707L
  ),

  check_dt_argument_laziness = diff_case(
    "Table-check optional-argument laziness, forcing order, and error priority",
    function() {
      parameter_set <- paradox::ps(x = paradox::p_dbl(0, 1))
      observe_order <- function(table) {
        state <- new.env(parent = emptyenv())
        state$events <- character()
        mark <- function(name, value) {
          state$events <- c(state$events, name)
          value
        }
        value <- parameter_set$check_dt(
          table,
          check_strict = mark("check_strict", TRUE),
          presence = mark("presence", "none"),
          allow_token = mark("allow_token", TRUE)
        )
        list(value = value, events = state$events)
      }
      list(
        zero_columns = observe_order(data.frame()),
        zero_rows = observe_order(data.frame(x = numeric())),
        one_row = observe_order(data.frame(x = 0.5)),
        first_error = observe_call(parameter_set$check_dt(
          data.frame(x = 0.5),
          check_strict = stop("forced check_strict"),
          presence = stop("forced presence"),
          allow_token = stop("forced allow_token")
        ))
      )
    },
    seed = 717L
  ),

  diagnostics = diff_case(
    "Stable condition classes/messages for representative invalid calls",
    function() {
      list(
        inverted_bounds = observe_call(paradox::p_dbl(2, 1)),
        duplicate_ids = observe_call(paradox::ParamSet$new(list(x = paradox::p_int(), x = paradox::p_dbl()))),
        invalid_factor = observe_call(paradox::p_fct(c("a", "a"))),
        invalid_dependency = observe_call(paradox::ps(x = paradox::p_int(depends = absent == 1L))),
        malformed_values = observe_call({
          parameter_set <- paradox::ps(x = paradox::p_int())
          parameter_set$values <- list(unnamed = NULL)
        })
      )
    },
    seed = 808L
  )
)
