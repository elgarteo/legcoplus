#' Search meeting
#'
#' Fetches meetings of a given committee.
#'
#' @param committee_id The id of a committee, or a vector of IDs.
#' 
#' @param from Only fetch meetings on or after this date. Accepts character
#'   values in `'YYYY-MM-DD'` format, and objects of class `Date`, `POSIXt`,
#'   `POSIXct`, `POSIXlt` or anything else that can be coerced to a date with
#'   `as.Date()`. Defaults to `'1900-01-01'`.
#'
#' @param to Only meetings on or before this date. Accepts character values in
#'   `'YYYY-MM-DD'` format, and objects of class `Date`, `POSIXt`, `POSIXct`,
#'   `POSIXlt` or anything else that can be coerced to a date with `as.Date()`.
#'   Defaults to the current system date.
#'
#' @param verbose Defaults to `TRUE`.
#'
#' @export
#' 
search_meeting <- function(committee_id, from = "1990-01-01",
                           to = Sys.Date(), verbose = TRUE) {
  meet_id <- legco::meeting_committee(committee_id = committee_id, verbose = verbose)
  
  if (!is.null(meet_id)) {
    if (length(meet_id$MeetID) <= 15) {
      df <- legco::meeting(meet_id$MeetID, verbose = verbose)
    } else {
      df <- data.frame()
      for (i in 1:ceiling(length(meet_id$MeetID) / 15)) {
        starting <- 15 * (i - 1) + 1
        ending <- ifelse(starting + 15 < length(meet_id), starting + 15, length(meet_id))
        tmp <- legco::meeting(meet_id$MeetID[starting:ending], verbose = verbose)
        df <- rbind(df, tmp)
      }
    }
    
    if (is.null(df)) {
      message(paste0("No meeting found for Committee ID ", committee_id, "."))
    } else {
      if (verbose) {
        message(paste(nrow(df), "record(s) match(es) your parameters."))
      }
      
      df
    }
  }
}

#' @rdname search_meeting
#' @export
legco_search_meeting <- search_meeting
