#' Search Division
#'
#' Fetch division(s) (votes) conducted by a given comittee(s) or on a given date
#' or during a given meeting slot. Note that only voting records from the
#' Council, House Committee, Finance Committee and its subcomittees are
#' available.
#'
#' @param target_date The date and time when the division(s) was/were conducted.
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
search_division <- function(target_date = NULL, committee_id = NULL, slot_id = NULL, 
                          verbose = TRUE) {
  if (is.null(target_date) & is.null(committee_id) & is.null(slot_id)) {
    message("Error: Please specifiy the date, a committee or a meeting slot.")
  } else {
    if (!is.null(target_date)) {
      target_date <- as.Date(target_date)
      from <- paste0(target_date, "T00:00:00")
      to <- paste0(target_date, "T23:59:59")
    }
    
    if (!is.null(slot_id)) {
      if (length(slot_id > 1)) {
        message("Error: Please enter only one Slot ID.")
      } else {
        tmp <- legco::meeting_committee(slot_id, verbose = verbose)
        from <- as.Date(tmp$StartDateTime)
        to <- as.Date(tmp$StartDateTime)
        committee_id <- tmp$CommitteeID
      }
    }
    
    if (!is.null(committee_id)) {
      tmp <- legco::committee(committee_id, verbose = verbose)
      committee_name <- tmp$NameEng
      committee_term <- tmp$TermID
    } else {
      committee_name <- NULL
      committee_term <- NULL
    }
    
    df <- legco::voting_record(committee = committee_name, term_id = committee_term, 
                               from = from, to = to, verbose = verbose)
    
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
        message(nrow(df), "record(s) match(es) your parameters.")
      }
      
      df
    }
  }
}

#' @rdname search_division
#' @export
legco_search_division <- search_division
