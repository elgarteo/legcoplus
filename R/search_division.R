#' Search Division
#'
#' Fetch division(s) (votes) conducted by a given comittee(s) or on a given date
#' or during a given meeting slot. Note that only voting records from the
#' Council, House Committee, Finance Committee and its subcomittees are
#' available.
#'
#' @param search_date The date and time when the division(s) was/were conducted.
#'   Accepts character values in `'YYYY-MM-DD'` format, and objects of class
#'   `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or anything else that can be coerced
#'   to a date with `as.Date()`.
#'
#' @param committee_id The id of a committee, or a vector of IDs.
#'
#' @param slot_id The id of a meeting slot. If `NULL`, returns voting records of
#'   all committee meetings.
#'
#' @param verbose Defaults to `TRUE`.
#'
#' @export
#' 
search_division <- function(search_date = NULL, committee_id = NULL, slot_id = NULL, 
                          verbose = TRUE) {
  if (is.null(search_date) & is.null(committee_id) & is.null(slot_id)) {
    stop("Please specifiy the date, a committee or a meeting slot.")
  }
  
  if (!is.null(search_date)) {
    search_date <- as.Date(search_date)
    from <- paste0(search_date, "T00:00:00")
    to <- paste0(search_date, "T23:59:59")
  } else {
    from <- "1900-01-01"
    to <- Sys.Date()
  }
  
  if (!is.null(slot_id)) {
    if (length(slot_id) > 1) {
      stop("Please enter only one Slot ID.")
    }
    tmp <- legco::meeting_committee(slot_id, verbose = verbose)
    from <- as.Date(tmp$StartDateTime)
    to <- as.Date(tmp$StartDateTime)
    committee_id <- tmp$CommitteeID
  }
  
  if (!is.null(committee_id)) {
    tmp <- legco::committee(committee_id, verbose = verbose)
    committee_name <- tmp$NameEng
    committee_term <- tmp$TermID
  } else {
    committee_name <- NULL
    committee_term <- NULL
  }
  
  output <- capture.output({df <- legco::voting_record(committee = committee_name, term_id = committee_term, 
                                                from = from, to = to, verbose = verbose)}, type = "message")
  
  n <- stringr::str_extract(output[2], "\\. [0-9]+")
  n <- as.numeric(gsub("\\. ", "", n))
  
  if (n > 10000) {
    df <- legco::voting_record(committee = committee_name, term_id = committee_term, 
                               from = from, to = to, verbose = verbose, n = n)
  } else {
    message(output[1])
    message(output[2])
  }
  
  if (!is.null(df)) {
    df <- df[c("VoteTime", "Committee", "TermID", "MotionEn", "MotionCh",
               "VoteSeparateMechanism", "GcPresentCount", "GcVoteCount",
               "GcYesCount", "GcNoCount", "GcAbstainCount", "GcResult",
               "FcPresentCount", "FcVoteCount", "FcYesCount", "FcNoCount",
               "FcAbstainCount", "FcResult", "OverallPresentCount",
               "OverallVoteCount", "OverallYesCount", "OverallNoCount",
               "OverallAbstainCount", "OverallResult")]
    df <- df[!duplicated(df$VoteTime), ]
    
    if (verbose) {
      message(nrow(df), " record(s) match(es) your parameters.")
    }
    
    df
  }
}

#' @rdname search_division
#' @export
legco_search_division <- search_division
