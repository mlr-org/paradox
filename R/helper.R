#' @title transpose
#'
#' @description
#' Converts [data.table::data.table] into a list of lists of points, possibly
#' removes `NA` entries of inactive parameter values due to unsatisfied
#' dependencies, and possibly calls the `trafo` function of the [ParamSet].
#'
#' @param data ([data.table::data.table])\cr
#' Rows are points and columns are parameters.
#'
#' @param ps ([ParamSet])\cr
#' If `trafo = TRUE`, used to call trafo function.
#'
#' @param filter_na (`logical(1)`)\cr
#' Should `NA` entries of inactive parameter values be removed due to
#' unsatisfied dependencies?
#'
#' @param trafo (`logical(1)`)\cr
#' Should the `trafo` function of the [ParamSet] be called?
transpose = function(data, ps = NULL, filter_na = TRUE, trafo = TRUE) {
  assert_data_table(data)
  assert_flag(filter_na)
  assert_flag(trafo)
  xs = transpose_list(data)
  if (filter_na) {
    xs = map(xs, function(x) Filter(Negate(is_scalar_na), x))
  }
  if (!is.null(ps) && trafo) {
    if (ps$has_trafo) xs = map(xs, function(x) ps$trafo(x, ps))
  }
  return(xs)
}

# TODO: the following may get simpler if we build a ParamSetCollection and copy from there
ps_union = function(sets) {
  assert_list(sets, types = "ParamSet")
  assert_names(discard(map_chr(sets, "set_id"), `==`, ""), type = "unique")
  ps = ParamSet$new()
  setinfo = map(unname(sets), function(s) {
    si = list(
      trafo = s$trafo,
      psids = names(s$params),
      forward_name_translation = structure(names(s$params), names = names(s$params)),
      set_id = s$set_id
    )
    if (s$set_id != "") {
      sid = s$set_id
      deps = copy(s$deps)
      s = ParamSet$new(map(s$params, function(p) {  # this works even if s is a PSC
        p = p$clone()
        p$id = sprintf("%s.%s", sid, p$id)
        p
      }))
      names(si$forward_name_translation) = names(s$params)
      si$psids = names(s$params)
      map(transpose_list(deps), function(d) {
        s$add_dep(id = sprintf("%s.%s", sid, d$id), on = sprintf("%s.%s", sid, d$on), cond = d$cond)
      })
    }
    trtmp = s$trafo
    on.exit({s$trafo = trtmp})
    s$trafo = NULL
    ps$add(s)
    si
  })
  if (any(map_lgl(sets, "has_trafo"))) {
    allnames = unlist(map(setinfo, "psids"), use.names = FALSE)
    ps$trafo = crate(function(x, param_set) {
      res = c(
        list(x[setdiff(names(x), allnames)]),
        map(setinfo, function(s) {
          trafo = s$trafo
          pv = x[match(names(s$forward_name_translation), names(x), nomatch = 0)]
          if (!is.null(trafo)) {
            # retrieve sublist of the set, assign it in set (after removing prefix)
            names(pv) = s$forward_name_translation[names(pv)]
            pv = trafo(pv)
            if (s$set_id != "") {
              names(pv) = sprintf("%s.%s", s$set_id, names(pv))
            }
          }
          pv
        })
      )
      res = unlist(res, recursive = FALSE)
      res[c(intersect(names(res), names(x)), setdiff(names(res), names(x)))]  # unchanged parameter names stay in order
    }, setinfo, allnames)
  }
  ps
}

# Put a function in a "lean" environment that does not carry unnecessary baggage with it (e.g. references to datasets)
#
# @param .fn: function to crate
# @param ...: the objects, which should be visible inside `.fn`. These objects should be given as is, not in a string
#   or expression or anything like that.
#
# @examples
# meta_f = function(z) {
#   x = 1
#   y = 2
#   crate(function() {
#     c(x, y, z)
#   }, x)
# }
# x = 100
# y = 200
# z = 300
# f = meta_f(1)
# f()
# #> [1]   1 200 300
# ## using `x` from the crate, `y` and `z` from global env
crate = function(.fn, ...) {
  environment(.fn) = list2env(parent = .GlobalEnv,
    structure(list(...), names = map_chr(substitute(list(...)), as.character)[-1]))
  .fn
}

# Get the R6ClassGenerator (constructor) by class name, even if the generator itself is not directly visible.
# This is necessary to create new instances of a `Param`, because the `Param` only contains a link to its class
# name, not its constructor.
# @param name: name of R6 class to be found
# @param env: environment (and its parents) to prefer
# Uses `getAnywhere()` to find objects named `name` with a `$new` slot that is a function. An error is thrown if nothing
# is found.
# If multiple objects are found, the order in which they are returned is:
# 1. object found in `env` (or its parents)
# 2. objects that are "visible" from the global environment
# 3. objects that are actually R6-objects (and do not just have a `$new()` function
get_r6_constructor = function(name, env = parent.frame()) {
  # data.table with <name>, <objs>, <where>, <visible>, <dups>
  candidates = do.call(data.table, getAnywhere(name))
  # reducing to columns: <objs>, <visible>
  candidates = candidates[!dups | visible, .(objs, visible)]

  # prefer object we find directly where the call expects it
  candidates[, found_in_env := FALSE]
  direct_found = tryCatch(get(name, envir = env), error = function(e) NULL)
  if (!is.null(direct_found)) {
    candidates = rbind(candidates, data.table(objs = list(direct_found), visible = TRUE, found_in_env = TRUE))
  }

  # throw away objects without a `$new()`
  candidates = candidates[map_lgl(objs, function(o) "new" %in% names(o) && is.function(o$new))]
  candidates[, isr6 := map_lgl(objs, is.R6Class)]

  if (!nrow(candidates)) {
    stopf("Could not find R6ClassGenerator (or any object with $new() function) named %s.", name)
  }
  # Order of preference:
  # found_in_env, then visible, then isr6
  candidates[order(found_in_env, visible, isr6, decreasing = TRUE)]$objs[[1]]
}
