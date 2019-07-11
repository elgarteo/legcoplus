#' Search Committee
#'
#' Search committee by full or partial name of committee.
#'
#' @param search_term The search term. Accept full or partial Chinese or English
#'   name of a committee.
#'
#' @param term_id The id of a term, or a vector of ids. If `NULL`, searches
#'   committees from all terms. Defaults to `NULL`.
#'
#' @param exact Whether to look for exact match of the search term. Defaults to
#'   `TRUE`. Defaults to `TRUE`.
#'
#' @param verbose Defaults to `TRUE`.
#'
#' @export
#' 
search_committee <- function(search_term, term_id = NULL, exact = TRUE, verbose = TRUE) {
  df <- legco::committee(verbose = verbose)
  
  if (grepl("[^\001-\177]", search_term)) { # Detect language of input
    # If Chinese
    if (!exact) {
      search_term <- unlist(strsplit(search_term, ""))
    }
    
    index <- sapply(search_term, function(x) grep(x, df$NameChi))
  } else {
    # If English
    term_tmp <- tolower(search_term)
    
    if (!exact) {
      term_tmp <- unlist(strsplit(term_tmp, " "))
    }
    
    index <- sapply(term_tmp, function(x) grep(x, df$NameEng, ignore.case = TRUE))
  }
  
  index <- unique(unlist(index))
  
  if (!length(index)) {
    stop("Could not find any matching result for search term \"", search_term, "\".")
  }
  
  df <- df[index, ]
  if (!is.null(term_id)) {
    df <- df[df$TermID %in% term_id, ]
  }
  rownames(df) <- 1:nrow(df)
  
  if (verbose) {
    message(nrow(df), " record(s) match(es) your parameters.")
  }
  
  df
}

#' @rdname search_committee
#' @export
legco_search_committee <- search_committee
