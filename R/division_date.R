#' Division by Date
#'
#' Fetch division(s) conducted on a specified date.
#'
#' @param target_date The date and time when the division(s) was/were conducted.
#'   Accepts character values in `'YYYY-MM-DD'` format, and objects of class
#'   `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or anything else that can be coerced
#'   to a date with `as.Date()`.
#'
#' @param committee_id The id of a committee, or a vector of IDs. If `NULL`,
#'   returns voting records of all committees. Note that not all committees have
#'   its voting records available. Defaults to `NULL`.
#'
#' @param meet_id The id of a meeting. If `NULL`, returns voting records of all
#'   committee meetings. Note that not all committees have its voting records
#'   available. Defaults to `NULL`.
#'
#' @param verbose Defaults to `TRUE`.
#'
#' @export
#' 
division_date <- function(target_date = NULL, committee_id = NULL, meet_id = NULL, 
                          verbose = TRUE) {
  if (is.null(target_date)) {
    message("Error: Please specifiy the date.")
  } else {
    target_date <- as.Date(target_date)
    from <- paste0(target_date, "T00:00:00")
    to <- paste0(target_date, "T23:59:59")
    
    if (!is.null(meet_id)) {
      if (length(meet_id > 1)) {
        message("Error: Please enter only one Meet ID.")
      } else {
        tmp <- legco::meeting(meet_id, verbose = verbose)
        meeting_date <- tmp$StartDateTime
        meeting_date <- as.Date(meeting_date)
        from <- meeting_date
        to <- meeting_date
        tmp <- legco::meeting_committee(meet_id, verbose = verbose)
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

#' @rdname division_date
#' @export
legco_division_date <- division_date
