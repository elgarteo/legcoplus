#' Answered Questions in LegCo
#'
#' Fetch full text and answered questions put to the government by specific
#' LegCo member(s).
#'
#' @param speaker_id The Speaker ID of a member, or a vector of IDs.
#'
#' @param lang The language of hansard files to search from. `'en'` returns the
#'   English version. `'zh'` returns the Traditional Chinese version. Defaults
#'   to `'en'`.
#'
#' @param type The type of question. `'oral'` returns oral questions.
#'   `'written'` returns written questions. `'all'` returns all questions.
#'   Defaults to `'all'`.
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
#' @param floor Whether to fetch results from the floor version of the hansard
#'   files. The floor version is the first presented version of hansard file in
#'   the original language delivered by the speakers in LegCo. If `'TRUE'`, the
#'   language option is ignored. Defaults to `FALSE`.
#'
#' @param n The number of entry to fetch. Defaults to `50`.
#'
#' @param verbose Defaults to `TRUE`.
#'
#' @export
#' 
answered_questions <- function(speaker_id = NULL, type = "all", lang = "en", from = '1900-01-01', 
                                   to = Sys.Date(), floor = FALSE, n = 50, verbose = TRUE) {
  if (is.null(speaker_id)) {
    message("Error: Please specifiy at least one LegCo member.")
  } else {
    df <- legco::questions(speaker_id = speaker_id, type = type, lang = lang,
                           from = from, to = to, floor = floor, n = n, verbose = verbose)
    
    if (!is.null(df)) {
      n <- set_limit()
      for (i in 1:nrow(df)) {
        # Locate range of Rundown ID of question
        hansard_id <- legco::rundown(df$RundownID[i], verbose = verbose)
        n <- check_limit(n, verbose)
        hansard_id <- hansard_id$HansardID
        max_rundown_id <- legco::subjects(hansard_id = hansard_id, verbose = verbose)
        n <- check_limit(n, verbose)
        max_rundown_id <- max_rundown_id$RundownID[min(which(max_rundown_id$RundownID > df$RundownID[i]))] - 1

        # Fetch full text with Rundown IDs
        full_txt <- legco::rundown((df$RundownID[i] + 1):max_rundown_id, verbose = verbose)
        n <- check_limit(n, verbose)
        full_txt <- full_txt[order(full_txt$RundownID), ]
        
        # Identify Speaker ID of answering public officer
        answering_speaker_id <- full_txt$SpeakerID[!full_txt$SpeakerID %in% c(1, df$SpeakerID[i])][1]
        
        # Categorise by Speaker ID
        answer_txt <- full_txt$Content[full_txt$SpeakerID == answering_speaker_id]
        # Remarks made by LegCo appointment holders (e.g. President)
        misc_txt <- full_txt$Content[full_txt$SpeakerID %in% 1:5]
        question_txt <- full_txt$Content[!full_txt$Content %in% c(answer_txt, misc_txt)]
        
        df$AnsweringSpeakerID <- answering_speaker_id
        df$Question[i] <- list(question_txt)
        df$Answer[i] <- list(answer_txt)
        df$Misc[i] <- list(misc_txt)
      }
      
      df
      
    }
  }
}