#' @field tags (named `list()` of `character()`)\cr
#' Can be used to group and subset parameters.
#' Named with parameter IDs.
#' A [`ParamSetCollection`]'s and a [`ParamSetShadow`]'s tags are derived from
#' the sets they are built on, but may also be assigned directly: the
#' assignment becomes that object's own answer for the IDs it names, leaves the
#' contained sets untouched, and survives later changes to them. IDs it does
#' not name -- a parameter a contained set gains later, for instance -- stay
#' derived.
