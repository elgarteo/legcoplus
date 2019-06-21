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
#' @param meet_id The id of a meeting. If `NULL`, returns
#'   voting records of all committee meetings. Note that not all committees have
#'   its voting records available. Defaults to `NULL`.
#'
#' @param from Only fetch results from hansards of meetings on or after this
#'   date. Accepts character values in `'YYYY-MM-DD'` format, and objects of
#'   class `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or anything else that can be
#'   coerced to a date with `as.Date()`. Defaults to `'1900-01-01'`.
#'
#' @param to Only fetch results from hansards of meetings on or before this
#'   date. Accepts character values in `'YYYY-MM-DD'` format, and objects of
#'   class `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or anything else that can be
#'   coerced to a date with `as.Date()`. Defaults to the current system date.
#'
#' @param n The number of entry to fetch. Defaults to `50`.
#'
#' @param verbose Defaults to `TRUE`.
#'
#' @export
#' 
member_voting_record <- function(speaker_id = NULL, member_id = NULL, committee_id = NULL,
                                 meet_id = NULL, from = '1990-01-01', to = Sys.Date(),
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
                               name_ch = members, from = from, to = to, n = n, verbose = verbose)
    
    if (!is.null(df)) {
      df <- df[c("VoteTime", "Committee", "TermID", "MotionEn", "MotionCh", "NameCh", "NameEn", "Vote")]
      
      df
    }
  }
}

#' @rdname member_voting_record
#' @export
legco_member_voting_record <- member_voting_record
