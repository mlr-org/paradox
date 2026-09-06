# Build an acyclic, detached description for base equality machinery. R6
# environments contain inherited active bindings; letting
# all.equal.environment() evaluate those bindings can select the parent
# ParamSet reader for a ParamSetCollection and is not a meaningful semantic
# comparison in any case.
param_set_equality_view = function(param_set) {
  # Equality is cold, but its answer still has to describe one possible
  # generation. Active bindings below are intentionally convenient
  # presentation readers, not a multi-field transaction: reading them from a
  # caller-owned graph directly could combine fields around a pending
  # finalizer or a callback reached while a derived node heals. The deep-clone
  # transaction already owns the exact graph/policy/Shadow receipt needed
  # here. Traverse that isolated graph so the view remains coherent without
  # adding any work to ordinary ParamSet reads.
  param_set = param_set$clone(deep = TRUE)

  objects = list(param_set)
  states = 0L  # 0 = undiscovered, 1 = active path, 2 = complete
  edges = list()
  views = list()

  enter = function(index) {
    object = objects[[index]]
    collection = inherits(object, "ParamSetCollection")
    shadow = inherits(object, "ParamSetShadow")
    children = if (collection) {
      object$sets
    } else if (shadow) {
      list(origin = object$origin)
    } else {
      list()
    }
    edges[[index]] <<- children
    views[[index]] <<- list(
      class = class(object),
      assert_values = object$assert_values,
      params = as.data.frame(object$params),
      values = object$values,
      tags = object$tags,
      deps = as.data.frame(object$deps),
      # COLLECTION callbacks are derived from their children and SHADOW
      # callbacks are derived from their origins. Generated adapter closures
      # are implementation details; the graph edges below retain their full
      # semantic sources instead.
      constraint = if (collection || shadow) NULL else object$constraint,
      extra_trafo = if (collection || shadow) NULL else object$extra_trafo,
      edge_kind = if (collection) "sets" else if (shadow) "origin" else NULL,
      edge_names = names(children),
      edge_nodes = integer(length(children))
    )
    states[[index]] <<- 1L
  }

  enter(1L)
  stack_nodes = 1L
  stack_edges = 1L
  while (length(stack_nodes)) {
    depth = length(stack_nodes)
    node = stack_nodes[[depth]]
    edge = stack_edges[[depth]]
    children = edges[[node]]
    if (edge > length(children)) {
      states[[node]] = 2L
      stack_nodes = stack_nodes[-depth]
      stack_edges = stack_edges[-depth]
      next
    }

    child = children[[edge]]
    stack_edges[[depth]] = edge + 1L
    known = which(vapply(objects, identical, logical(1L), y = child))
    if (length(known)) {
      child_node = known[[1L]]
      if (states[[child_node]] == 1L) {
        stop("ParamSet capsule graph contains a cycle", call. = FALSE)
      }
    } else {
      child_node = length(objects) + 1L
      objects[[child_node]] = child
      states[[child_node]] = 0L
    }
    views[[node]]$edge_nodes[[edge]] = child_node

    if (states[[child_node]] == 0L) {
      enter(child_node)
      stack_nodes = c(stack_nodes, child_node)
      stack_edges = c(stack_edges, 1L)
    }
  }

  list(root = 1L, nodes = views)
}

#' Compare ParamSets by their detached semantic state
#'
#' Equality covers the complete capsule graph: node classes, the public
#' `assert_values` policy, parameters, values, tags, dependencies, base-node
#' callbacks, collection children, and Shadow origins. Independently built
#' equivalent graphs compare equal. Shared and duplicated child topology are
#' distinct, and a cycle is rejected. Generated collection and Shadow adapter
#' closures are representation details and are compared through their
#' authoritative child or origin state instead.
#'
#' @param target,current Objects to compare.
#' @param ... Additional arguments passed to [all.equal()].
#' @return `TRUE` or a character vector describing differences.
#' @export
all.equal.ParamSet = function(target, current, ...) {
  if (!inherits(current, "ParamSet")) {
    return(sprintf(
      "'current' is not a ParamSet, but %s",
      paste(class(current), collapse = "/")
    ))
  }
  if (identical(target, current)) {
    # Snapshot one object once. Apart from avoiding duplicate cold work, this
    # keeps reflexivity meaningful when cloning an opaque utility value has a
    # documented callback which mutates its source.
    view = param_set_equality_view(target)
    return(all.equal(view, view, ...))
  }
  all.equal(
    param_set_equality_view(target),
    param_set_equality_view(current),
    ...
  )
}
