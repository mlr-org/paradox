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

# Create a ParamSet from a list of ParamSets
# This emulates `ParamSetCollection$new(sets)`, except that
# - The result is a `ParamSet`, not a `ParamSetCollection`
# - The ParamSets are allowed to have `$trafo`, which are collected together into a single function.
# This emulates `ParamSetCollection$new(sets)`, which in particular means that the resulting ParamSet has all the Params
# from the input `sets`, but some `$id`s are changed: If the ParamSet has a non-empty `set_id`, then the Params will
# have their <id> changed to <set_id>.<id>. This is also reflected in deps and in `$trafo`.
# @param sets: list of ParamSet
ps_union = function(sets) {
  assert_list(sets, types = "ParamSet")
  assert_names(discard(map_chr(sets, "set_id"), `==`, ""), type = "unique")

  psc = ParamSetCollection$new(map(sets, function(x) {
    if (x$has_trafo) {
      # PSC can not use ParamSet with a `$trafo` that is set.
      x = x$clone()
      x$trafo = NULL
    }
    x
  }))

  newps = ParamSet$new()$add(psc)

  # This loop collects information that is needed by the trafo.
  # Resulting is a list of named lists, with one element per `sets` entry. Elements of the named lists are:
  # - trafo: trafo of the given ParamSet
  # - set_id: set_id of the given ParamSet
  # - forward_name_translation: named `character`. Names are the Param IDs of the resulting newps,
  #   values are the Param IDs of the original Params in the `sets` argument.
  #   E.g. if a single ParamSet with set_id "sid" and with one Param with id "pid" is given,
  #   then this is a `c(sid.pid = "pid")`.
  #   Why is this needed? If the $trafo() is given a value `list(sid.pid = 1)`, then
  #   `forward_name_translation` can be used to rename this to `list(pid = 1)`, which is what the
  #   original trafo expects.
  setinfo = map(unname(sets), function(s) {
    sparams = s$params  # avoid slow ParamSetCollection $params active binding
    sinfo = list(
      trafo = s$trafo,
      set_id = s$set_id,
      forward_name_translation = names2(sparams)
    )
    psids = names2(sparams)
    if (s$set_id != "") {
      psids = sprintf("%s.%s", s$set_id, psids)
    }
    names(sinfo$forward_name_translation) = psids
    sinfo
  })

  if (any(map_lgl(sets, "has_trafo"))) {
    # allnames: names of all parameters, as seen from the outside
    allnames = names2(unlist(map(setinfo, "forward_name_translation")))
    assert_set_equal(allnames, names2(newps$params))  # this should always be the case

    newps$trafo = crate(function(x, param_set) {
      res = unlist(map(setinfo, function(s) {
        trafo = s$trafo
        # get the parameter values that the current trafo should operate on,
        # as identified by the names in forward_name_translation
        pv = x[match(names(s$forward_name_translation), names(x), nomatch = 0)]
        if (!is.null(trafo)) {
          # translate name from "<set_id>.<param_id>" to "<param_id>"
          names(pv) = s$forward_name_translation[names(pv)]
          pv = trafo(pv)

          # append prefix again. trafo() could have changed parameter names, so
          # we can't use any cached name_translation magic here
          if (s$set_id != "") {
            names(pv) = sprintf("%s.%s", s$set_id, names(pv))
          }
        }
        pv
      }), recursive = FALSE)

      # add the Params that were not translated at all, because the ParamSet doesn't know about them.
      res = c(remove_named(x, allnames), res)

      res[c(intersect(names(res), names(x)), setdiff(names(res), names(x)))]  # unchanged parameter names stay in order
    }, setinfo, allnames)
  }
  newps
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
  found_in_env = dups = visible = isr6 = NULL  # pacify static check
  # data.table with <name>, <objs>, <where>, <visible>, <dups>
  candidates = do.call(data.table, utils::getAnywhere(name))
  # reducing to columns: <objs>, <visible>
  candidates = candidates[!dups | visible, list(objs, visible)]

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
