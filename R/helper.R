repr = function(x) {
  str_collapse(utils::capture.output(print(x)), "\n")
}

as_type = function(x, type) {
  switch(type,
    logical = as.logical(x),
    integer = as.integer(x),
    numeric = as.numeric(x),
    character = as.character(x),
    list = as.list(x),
    stopf("Invalid storage type '%s'", type)
  )
}

.paradox_srcref_attributes = c("srcref", "srcfile", "wholeSrcref")

.paradox_is_srcref_syntax_node = function(x) {
  typeof(x) == "closure" ||
    is.language(x) ||
    is.pairlist(x) ||
    is.atomic(x)
}

.paradox_node_has_srcref = function(x) {
  for (name in .paradox_srcref_attributes) {
    if (!is.null(attr(x, name, exact = TRUE))) {
      return(TRUE)
    }
  }
  FALSE
}

.paradox_has_srcref_node = function(x) {
  if (missing(x)) {
    return(FALSE)
  }
  # Calls can contain arbitrary R objects when assembled programmatically.
  # Environments and other reference-bearing leaves are payload, not syntax:
  # even assigning an attribute would mutate such an object in place.
  if (!.paradox_is_srcref_syntax_node(x)) {
    return(FALSE)
  }
  if (.paradox_node_has_srcref(x)) {
    return(TRUE)
  }
  if (typeof(x) == "closure") {
    if (.paradox_has_srcref_node(formals(x))) {
      return(TRUE)
    }
    return(.paradox_has_srcref_node(body(x)))
  }

  if (!is.language(x) && !is.pairlist(x)) {
    return(FALSE)
  }
  if (is.symbol(x)) {
    return(FALSE)
  }
  for (i in seq_len(length(x))) {
    if (typeof(x[[i]]) == "symbol" && !nzchar(as.character(x[[i]]))) {
      next
    }
    if (.paradox_has_srcref_node(x[[i]])) {
      return(TRUE)
    }
  }
  FALSE
}

.paradox_has_srcref = function(x) {
  if (typeof(x) == "closure" || is.language(x) || is.pairlist(x)) {
    .paradox_has_srcref_node(x)
  } else {
    FALSE
  }
}

.paradox_strip_srcref_node = function(x) {
  if (!.paradox_is_srcref_syntax_node(x)) {
    return(x)
  }
  retained_attributes = attributes(x)
  if (!is.null(retained_attributes)) {
    retained_attributes[.paradox_srcref_attributes] = NULL
  }

  if (typeof(x) == "closure") {
    stripped = x
    original_formals = formals(x)
    if (.paradox_has_srcref_node(original_formals)) {
      formals(stripped) = .paradox_strip_srcref_node(original_formals)
    }
    original_body = body(x)
    if (.paradox_has_srcref_node(original_body)) {
      body(stripped) = .paradox_strip_srcref_node(original_body)
    }
  } else if (is.language(x) || is.pairlist(x)) {
    if (is.symbol(x)) {
      stripped = x
    } else {
      stripped = x
      for (i in seq_len(length(x))) {
        if (typeof(x[[i]]) == "symbol" && !nzchar(as.character(x[[i]]))) {
          next
        }
        child = x[[i]]
        # `[[<- NULL` removes a call/pairlist cell. Rebuild only syntax nodes
        # that actually carry source metadata; this also avoids rewriting the
        # empty formal pairlist of a nested `function()`.
        if (.paradox_has_srcref_node(child)) {
          stripped[[i]] = .paradox_strip_srcref_node(child)
        }
      }
    }
  } else {
    stripped = x
  }

  # Replacement functions such as `body<-` may discard closure attributes.
  # Restore every non-source attribute in its original order after rebuilding.
  attributes(stripped) = retained_attributes
  stripped
}

.paradox_strip_srcref = function(x) {
  # NULL is the overwhelmingly common constructor callback value. Keep this
  # before the option lookup and recursive scanner: source normalization is an
  # admission feature, not a tax on callback-free Domain construction.
  if (is.null(x)) {
    return(x)
  }
  if (typeof(x) != "closure" && !is.language(x) && !is.pairlist(x)) {
    return(x)
  }
  if (!isTRUE(getOption("paradox.strip_srcrefs", TRUE)) ||
      !.paradox_has_srcref_node(x)) {
    return(x)
  }
  .paradox_strip_srcref_node(x)
}
