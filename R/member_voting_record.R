#' Voting Records of a LegCo Member
#'
#' Fetch voting records of a specific LegCo member.
#'
#' @param speaker_id The Speaker ID, or a vector of IDs. If `NULL`, returns all
#'   LegCo members. Defaults to `NULL`.
#'
#' @param member_id The Member ID, or a vector of IDs. Defaults to `NULL`.
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
member_voting_record <- function(speaker_id = NULL, member_id = NULL, from = '1900-01-01',
                                 to = Sys.Date(), n = 50, verbose = TRUE) {
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
    
    df <- legco::voting_record(name_ch = members, from = from, to = to, n = n, verbose = verbose)
    df <- df[c("VoteTime", "Committee", "TermID", "MotionEn", "MotionCh", "NameCh", "NameEn", "Vote")]
    
    df
    
  }
}