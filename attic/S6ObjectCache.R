
S6Cache = R6Class("S6Cache",
  cloneable = FALSE,
  public = list(
    initialize = function(cachesize) {
      private$.cache = new.env(parent = emptyenv())
      private$.evictable = list()
      private$.insertpointer = 1
      private$.evictpointer = 1
      private$.size = 0
      private$.maxsize = cachesize
      private$.s6ns = asNamespace("set6")
      private$.settypes = keep(names(private$.s6ns), function(n) {
        obj = private$.s6ns[[n]]
        if (!is.environment(obj) || !is.function(obj[["get_inherit"]])) return(FALSE)
        repeat {
          if (identical(obj, set6::Set)) return(TRUE)
          obj = obj$get_inherit()
          if (is.null(obj)) return(FALSE)
        }
      })

    },
    get = function(description) {
      assert_string(description)
      description = trimws(description)
      set = private$.cache[[description]]
      if (!is.null(set)) return(set)
      set =
        private$.infer_interval(description) %??%
        private$.infer_discrete(description) %??%
        private$.infer_settype(description) %??%
        if (grepl("^n", description)) {
          stype = private$.infer_settype(description)
          if (!is.null(stype)) {
            set6::setpower(stype, "n")
          }
        } %??% stopf("Description '%s' not in set6_cache and could not be constructed.", description)  # how do we automatically generate a set from a string?
      self$enter(self$canonicalize(set), description)
      set
    },
    canonicalize = function(object) {
      if (test_string(object, pattern = "[^ ]")) return(self$get(object) %??% stopf("Could not get object from string '%s'", object))
      assert_class(object, "Set")
      repr = self$set6_sane_repr(object)
      private$.cache[[repr]] %??% object  # don't use the given object when one with the same representation is already present.
    },
    enter = function(object, description = NULL) {
      # if we were able to get the object from the description, then we mark it as evictable
      private$.enter_lowlevel(object, c(description, self$set6_sane_repr(object)), evictable = TRUE)
    },
    set6_sane_repr = function(object) {
      assert_r6(object, "Set")
      using_unicode_for_whatever_reason = set6::useUnicode()
      on.exit(set6::useUnicode(using_unicode_for_whatever_reason))
      set6::useUnicode(FALSE)
      object$strprint(n = 1e10)
    }
  ),
  private = list(
    .cache = NULL,
    .size = NULL,
    .maxsize = NULL,

    .evictable = NULL,  # list indicating names of objects that can be evicted
    .insertpointer = NULL,
    .evictpointer = NULL,

    .s6ns = NULL,  # namespace of set6, from where we get the Set constructors
    .settypes = NULL,  # names of the set6 constructors
    .enter_lowlevel = function(object, descriptions, evictable) {
      while (private$.size >= private$.maxsize) {
        # evict
        if (private$.evictpointer == private$.insertpointer) {  # can't evict
          if (evictable) {
            return(FALSE)  # we are full, have nothing to evict, but *this object* is evictable, so don't bother.
          } else {
            break
          }
        }
        if (private$.evictpointer > private$.maxsize + 1) {
          private$.evictpointer = 1
          private$.evictable = private$.evictable[seq_len(min(private$.maxsize, 1.5 * (private$.insertpointer - 1)))]
        }
        evp = private$.evictpointer
        evicting = private$.evictable[[evp]]
        assert_character(evicting, min.len = 1, any.missing = FALSE)
        rm(list = evicting, envir = private$.cache)
        private$.size = private$.size - 1
        private$.evictpointer = evp + 1
        private$.evictable[evp] = list(NULL)  # remove list element, but don't shorten list
      }

      for (desc in descriptions) {
        private$.cache[[desc]] = object
      }

      private$.size = private$.size + 1  # even if it has multiple descriptions, the object only counts for one.
      if (evictable) {
        ins = private$.insertpointer
        if (ins > private$.maxsize + 1) {
          ins = 1
        }
        private$.evictable[[ins]] = descriptions
        private$.insertpointer = ins + 1
      }
    },
    .get_settype = function(st, description, error_on_not_found = TRUE, construction_args = list()) {
       matches = private$.settypes[match(tolower(st), tolower(private$.settypes), nomatch = 0)]
       if (!length(matches)) {
         if (error_on_not_found) {
           stopf("Unknown settype '%s' in description '%s'", st, description)
         } else {
           return(NULL)
         }
       }
       if (length(matches) > 1) {
         matches = private$.settypes[match(st, private$.settypes, nomatch = 0)]
         # even error if error_on_not_found is FALSE, because this is a special message
         if (length(matches) != 1) stopf("Settype '%s' in description '%s' matched multiple set6 classes, but none of them matches by case.",st, description)
       }
       do.call(private$.s6ns[[matches]]$new, construction_args)
    },
    .infer_interval = function(description) {
      intervaldef = regmatches(description, regexec("^([[(])(([^,]*),([^,|]*))?(\\| *([a-zA-Z]+))? *([])]) *(^ *[0-9n]+)?$", description))[[1]]
      if (!length(intervaldef)) return(NULL)
      type = paste0(intervaldef[[2]], intervaldef[[8]])
      lower = if (intervaldef[[4]] == "") -Inf else suppressWarnings(as.numeric(intervaldef[[4]]))
      upper = if (intervaldef[[5]] == "") Inf else suppressWarnings(as.numeric(intervaldef[[5]]))
      if (is.na(lower) || is.na(upper)) stopf("Description '%s' is ill-formed interval expression.", description)
      universe = if (intervaldef[[7]] == "") set6::ExtendedReals$new() else private$.get_settype(intervaldef[[7]], description)
      result = set6::Interval$new(lower = lower, upper = upper, type = type, class = universe$class, universe = universe)
      private$.make_power(result, intervaldef[[9]], description)
    },
    .infer_discrete = function(description) {
      discretedef = regmatches(description, regexec("^\\{([^{}|]*)(\\| *([a-zA-Z]+))? *\\} *(^ *[0-9n]+)?$", description))[[1]]
      if (!length(discretedef)) return(NULL)
      if (discretedef[[2]] == "") {
        entries = character(0)
      } else {
        entries = strsplit(discretedef[[2]], " *, *")[[1]]
        if ("" %in% entries) stopf("Empty string element not allowed in description '%s'", description)
      }
      universe = if (discretedef[[4]] == "") set6::Universal$new() else private$.get_settype(discretedef[[4]], description)
      result = set6::Set$new(elements = entries, universe = universe)
      private$.make_power(result, discretedef[[5]], description)
    },
    .infer_settype = function(description) {
      typedef = regmatches(description, regexec("^([^^ ]*) *(^ *[0-9n]+)?$", description))[[1]]
      if (!length(typedef)) return(NULL)
      result = private$.get_settype(typedef[[2]], description, error_on_not_found = FALSE)
      if (is.null(result)) return(NULL)
      private$.make_power(result, typedef[[3]], description)
    },
    .make_power = function(set, powerexp, description) {
      if (powerexp == "") {
        return(set)
      }
      power = substr(powerexp, 2, nchar(powerexp))
      if (trimws(power) == "n") {
        power = "n"
      } else {
        power = suppressWarnings(as.numeric(power))
        if (is.na(power)) stopf("Ill-formatted power expression in %s", description)
      }
      return(set6::setpower(set, power))
    }
  )
)$new(cachesize = 2^20)

