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

crate = function(.fn, ...) {
  environment(.fn) = list2env(parent = .GlobalEnv,
    structure(list(...), names = map_chr(substitute(list(...)), as.character)[-1]))
  .fn
}
