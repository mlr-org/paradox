% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Domain_methods.R
\name{domain_check}
\alias{domain_check}
\alias{domain_assert}
\alias{domain_test}
\title{Check Value Validity}
\usage{
domain_check(param, values, internal = FALSE)

domain_assert(
  param,
  values,
  internal = FALSE,
  .var.name = checkmate::vname(param),
  add = NULL
)

domain_test(param, values)
}
\arguments{
\item{param}{(\code{Domain}).}

\item{values}{(\code{any}).}

\item{internal}{(\code{logical(1)})\cr
When set, function arguments are not checked for plausibility and \code{special_values} are not respected.
This is an optimization for internal purposes and should not be used.}
}
\value{
If successful \code{TRUE}, if not a string with the error message.
}
\description{
\pkg{checkmate}-like check-function. Check whether a list of values is feasible in the domain.
A value is feasible if it is of the same \code{storage_type}, inside of the bounds or element of
\code{special_vals}. \code{TuneToken}s are generally \emph{not} accepted, so they should be filtered out
before the call, if present.

\code{domain_check} will return \code{TRUE} for accepted values, a \code{character(1)} error message otherwise.

\code{domain_test} will return \code{TRUE} for accepted values, \code{FALSE} otherwise.

\code{domain_assert} will return the \code{param} argument silently for accepted values, and throw an error message otherwise.
}
\keyword{internal}
