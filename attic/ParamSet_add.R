
    #' @description
    #' Adds a single param or another set to this set, all params are cloned on-demand,
    #' so the input does not need to be cloned.
    #'
    #' @param p ([Param] | [ParamSet]).
    add = function(p) {

      assert_multi_class(p, c("Param", "ParamSet"))
      if (inherits(p, "Param")) { # level-up param to set
        pparams = structure(list(p), names = p$id)
        if (!is.null(private$.tags)) ptags = structure(list(p$param_tags), names = p$id)
        ptrafo = NULL
        pvalues = NULL
        pdeps = NULL
      } else {
        pparams = p$params_unid
        if (!is.null(private$.tags)) ptags = p$tags
        ptrafo = p$trafo
        pvalues = p$values
        pdeps = p$deps
      }

      nn = c(names(private$.params_unid), names(pparams))
      assert_names(nn, type = "strict")
      if (!is.null(ptrafo)) {
        stop("Cannot add a param set with a trafo.")
      }
      private$.params_unid = c(private$.params_unid, pparams)
      private$.values = c(private$.values, pvalues)
      if (!is.null(private$.tags)) private$.tags = c(private$.tags, ptags)
      private$.deps = rbind(private$.deps, pdeps)
      invisible(self)
    },

############## ParamSetCollection

    #' @description
    #' Adds a set to this collection.
    #'
    #' @param p ([ParamSet]).
    add = function(p) {
      assert_r6(p, "ParamSet")
      setnames = names(private$.sets) %??% map_chr(private$.sets, "set_id")
      if (p$set_id == "") {
        unnamed_set_parnames = map(private$.sets[setnames == ""], function(x) names(x$params_unid))
      } else if (p$set_id %in% setnames) {
        stopf("Setid '%s' already present in collection!", p$set_id)
      }
      if (p$has_trafo) {
        stop("Building a collection out sets, where a ParamSet has a trafo is currently unsupported!")
      }
      pnames = names(p$params_unid)
      nameclashes = intersect(
        ifelse(p$set_id != "", sprintf("%s.%s", p$set_id, pnames), pnames),
        names(self$params_unid)
      )
      if (length(nameclashes)) {
        stopf("Adding parameter set would lead to nameclashes: %s", str_collapse(nameclashes))
      }
      set_addition = list(p)
      if (!is.null(names(private$.sets))) {
        # ignoring the other ParamSet's set_id in favor of names(private$.sets), so add the name here as well.
        names(set_addition) = p$set_id
      }

      tagsaddition = p$tags
      names(tagsaddition) = sprintf("%s.%s", p$set_id, names(tagsaddition))
      private$.tags = c(private$.tags, tagsaddition)

      private$.sets = c(private$.sets, set_addition)
      invisible(self)
    },
