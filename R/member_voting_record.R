#' Voting Records of a LegCo Member
#'
#' Fetch voting records of specific LegCo member(s).
#'
#' @param speaker_id The Speaker ID, or a vector of IDs. If `NULL`, returns all
#'   LegCo members. Defaults to `NULL`.
#'
#' @param member_id The Member ID, or a vector of IDs. If `NULL`, returns all
#'   LegCo members. Defaults to `NULL`.
#'
#' @param committee_id The id of a committee, or a vector of IDs. If `NULL`,
#'   returns voting records of all committees. Note that not all committees have
#'   its voting records available. Defaults to `NULL`.
#'
#' @param slot_id The id of a meeting slot. If `NULL`, returns voting records of all
#'   committee meetings. Note that not all committees have its voting records
#'   available. Defaults to `NULL`.
#'
#' @param from Only fetch votes conducted at or after this time. Accepts
#'   character values in `'YYYY-MM-DDTHH:MM:SS'` format, and objects of class
#'   `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or anything else that can be coerced
#'   to a time with `as.POSIXlt()`. Defaults to `'1900-01-01T00:00:00'`.
#'
#' @param to Only fetch votes conducted at or before this time. Accepts
#'   character values in `'YYYY-MM-DDTHH:MM:SS'` format, and objects of class
#'   `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or anything else that can be coerced
#'   to a time with `as.POSIXlt()`. Defaults to system time.
#'
#' @param n The number of entry to fetch. Defaults to `50`.
#'
#' @param verbose Defaults to `TRUE`.
#'
#' @export
#' 
member_voting_record <- function(speaker_id = NULL, member_id = NULL, committee_id = NULL,
                                 slot_id = NULL, from = '1990-01-01T00:00:00', to = Sys.time(),
                                 n = 50, verbose = TRUE) {
  if (is.null(speaker_id) & is.null(member_id)) {
    message("Error: Please specifiy at least one LegCo member.")
  } else { 
    members <- {} 
    
    if (!is.null(speaker_id)) {
      tmp <- legco::speakers(speaker_id, verbose = verbose)
      members <- c(members, tmp$NameChi)
    }
    
    if (!is.null(member_id)) {
      tmp <- legco::member(member_id, verbose = verbose)
      tmp <- paste0(tmp$SurnameChi, tmp$FirstnameChi)
      members <- c(members, tmp)
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
                               name_ch = members, from = from, to = to, n = n, verbose = verbose)
    
    if (!is.null(df)) {
      df <- df[c("VoteTime", "Committee", "TermID", "MotionEn", "MotionCh", "OverallResult",
                 "NameCh", "NameEn", "Vote")]
      if (verbose) {
        message(paste(nrow(df), "record(s) match(es) your parameters."))
      }
      
      df
    }
  }
}

#' @rdname member_voting_record
#' @export
legco_member_voting_record <- member_voting_record
