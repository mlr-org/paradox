# Create a ParamSet from a list of ParamSets
# This emulates `ParamSetCollection$new(sets)`, except that
# - The result is a `ParamSet`, not a `ParamSetCollection`
# - The ParamSets are allowed to have `$trafo`, which are collected together into a single function.
# This emulates `ParamSetCollection$new(sets)`, which in particular means that the resulting ParamSet has all the Params
# from the input `sets`, but some `$id`s are changed: If the ParamSet has a non-empty `set_id`, then the Params will
# have their <id> changed to <set_id>.<id>. This is also reflected in deps and in `$trafo`.
# @param sets: list of ParamSet
ps_union = function(sets, tag_set = FALSE, tag_params = FALSE) {
  assert_list(sets, types = "ParamSet")

  if (!ignore_ids) {
    names(sets) = map(sets, "set_id")
  }

  psc = ParamSetCollection$new(sets, ignore_ids = TRUE)

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
  setinfo = unname(imap(keep(sets, function(x) x$has_trafo), function(s, n) {
    sparams = s$params_unid # avoid slow ParamSetCollection $params active binding
    sinfo = list(
      trafo = s$trafo,
      set_id = n,
      forward_name_translation = names2(sparams)
    )
    psids = names2(sparams)
    if (n != "") {
      psids = sprintf("%s.%s", n, psids)
    }
    names(sinfo$forward_name_translation) = psids
    sinfo
  }))

  if (length(setinfo)) {
    # allnames: names of all parameters, as seen from the outside
    allnames = names2(unlist(map(setinfo, "forward_name_translation")))
    assert_subset(allnames, names2(newps$params_unid))  # just check, this should always be the case

    newps$trafo = crate(function(x, param_set) {
      res = unlist(mlr3misc::map(setinfo, function(s) {
        trafo = s$trafo
        # get the parameter values that the current trafo should operate on,
        # as identified by the names in forward_name_translation
        pv = x[match(names(s$forward_name_translation), names(x), nomatch = 0)]

        # translate name from "<set_id>.<param_id>" to "<param_id>"
        names(pv) = s$forward_name_translation[names(pv)]
        pv = trafo(pv)

        # append prefix again. trafo() could have changed parameter names, so
        # we can't use any cached name_translation magic here
        if (s$set_id != "") {
          names(pv) = sprintf("%s.%s", s$set_id, names(pv))
        }

        pv
      }), recursive = FALSE)

      # add the Params that were not translated at all, because the ParamSet doesn't know about them.
      res = c(mlr3misc::remove_named(x, allnames), res)

      res[c(intersect(names(res), names(x)), setdiff(names(res), names(x)))]  # unchanged parameter names stay in order
    }, setinfo, allnames)
  }
  newps
}
