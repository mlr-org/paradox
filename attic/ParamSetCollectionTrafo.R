active = list(
    #' @field trafo
    trafo = function() {
      if (!self$has_trafo) return(NULL)
      sets = map(private$.sets, function(s) {
        psids = names(s$params)
        if (s$set_id != "") {
          psids = sprintf("%s.%s", s$set_id, psids)
        }
        list(
          set_id = s$set_id,
          trafo = s$trafo,
          psids = psids
        )
      })
      allnames = unlist(map(sets, "psids"))
      crate(function(x, param_set) {
        results = list()
        for (s in sets) {
          trafo = s$trafo
          pv = x[intersect(s$psids, names(x))]
          if (!is.null(trafo)) {
            # retrieve sublist for each set, then assign it in set (after removing prefix)
            if (s$set_id != "") {
              names(pv) = substr(names(pv), nchar(s$set_id) + 2, nchar(names(pv)))
            }
            pv = trafo(pv)
            if (s$set_id != "") {
              names(pv) = sprintf("%s.%s", s$set_id, names(pv))
            }
          }
          results[[length(results) + 1]] = pv
        }
        res <- c(x[setdiff(names(x), allnames)], unlist(results, recursive = FALSE))
        res[c(intersect(names(res), names(x)), setdiff(names(res), names(x)))]  # put the names of unchanged parameters to the front
      }, sets, allnames)
    },

    has_trafo = function() {
      any(map_lgl(private$.sets, "has_trafo"))
    }
)
