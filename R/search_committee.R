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
#' @param verbose Defaults to `TRUE`.
#'
#' @export
#' 
search_committee <- function(search_term, term_id = NULL, verbose = TRUE) {
  df <- legco::committee(verbose = verbose)
  
  if (length(grep("[^\001-\177]", search_term)) == 1) { # Detect language of input
    # If Chinese
    index <- which(df$NameChi %in% search_term)
    
    if (!length(index)) {
      index <- search_columns(search_term, "", df$NameChi)
    }
  } else {
    # If English
    name_tmp <- tolower(gsub("^ | $", "", df$NameEng))
    term_tmp <- tolower(gsub("^ | $", "", search_term))
    
    index <- which(df$NameEng %in% term_tmp)
    
    if (!length(index)) {
      index <- search_columns(term_tmp, " ", name_tmp)
    }
  }
  
  if (length(index) > 0) {
    df <- df[index, ]
    if (!is.null(term_id)) {
      df <- df[df$TermID %in% term_id, ]
    }
    rownames(df) <- 1:nrow(df)
  } else {
    message(paste0("Error: Could not find any matching result for search term \"", search_term, "\"."))
    df <- NULL
  }
  
  if (!is.null(df)) {
    if (verbose) {
      message(paste(nrow(df), "record(s) match(es) your parameters."))
    }
    
    df
  }
}

#' @rdname search_committee
#' @export
legco_search_committee <- search_committee
