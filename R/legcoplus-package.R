#' legcoplus: Improves the Hong Kong Legislative Council APIs
#'
#' Provides functions to present data from the Hong Kong Legislative Council
#' APIs in more usable forms.
#'
#' This package requires the package `'legco'` to run.
#'
#' In addition to the standard function names, each function in the `legco`
#' package has a wrapper where the name is prefixed with `'legco_'`. For
#' example, both `all_questions()` and `legco_all_questions()` will return the same
#' result. This is because function names are taken from the data endpoints
#' provided by the APIs on , which nonetheless are often not very informative
#' and could clash with functions in other packages (e.g. `all_questions()` is not a
#' term unique to LegCo).
#'
#' This package is not officially related to or endorsed by the Legislative
#' Council of Hong Kong.
#'
#' @docType package
#' @name legcoplus
#' @importFrom dplyr full_join
#' @import legco 
NULL
