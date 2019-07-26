#' Answered Questions in LegCo
#'
#' Fetch full text and answered questions put to the government by LegCo
#' member(s).
#'
#' @param speaker_id The Speaker ID, or a vector of IDs, as specified in the
#'   output from the function `legco::speakers()`. Defaults to `NULL`.
#'
#' @param member_id The member ID, or a vector of IDs, as specified in the
#'   output of the function `legco::member()`. Defaults to `NULL`.
#'
#' @param rundown_id The starting id of a rundown, or a vector of ids, as
#'   specified in the output of the function `legco::rundown()`. Defaults to
#'   `NULL`.
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
answered_questions <- function(speaker_id = NULL, member_id = NULL,
                               rundown_id = NULL, type = "all", lang = "en",
                               from = '1900-01-01', to = Sys.Date(), 
                               floor = FALSE, n = 50, verbose = TRUE) {
  if (is.null(speaker_id) & is.null(member_id) & is.null(rundown_id)) {
    stop("Please specifiy at least one LegCo member or Rundown ID.")
  } 
  
  if (!is.null(member_id)) {
    tmp <- search_member(member_id = member_id, verbose = verbose)
    speaker_id <- c(speaker_id, tmp$SpeakerID)
  }
  
  df <- legco::questions(speaker_id = speaker_id, rundown_id = rundown_id,
                         type = type, lang = lang, from = from,
                         to = to, floor = floor, n = n, verbose = verbose)
  
  if (!is.null(df)) {
    for (i in 1:nrow(df)) {
      # Locate range of Rundown ID of question
      hansard_id <- legco::rundown(df$RundownID[i], verbose = verbose)
      hansard_id <- hansard_id$HansardID
      max_rundown_id <- legco::subjects(hansard_id = hansard_id, verbose = verbose)
      max_rundown_id <- max_rundown_id$RundownID[min(which(max_rundown_id$RundownID > df$RundownID[i]))] - 1
      
      # Fetch full text with Rundown IDs
      full_txt <- legco::rundown((df$RundownID[i] + 1):max_rundown_id, verbose = verbose)
      full_txt <- full_txt[order(full_txt$RundownID), ]
      
      # Identify Speaker ID of answering public officer
      answering_speaker_id <- unique(full_txt$SpeakerID[full_txt$SpeakerID %in% 6:32])
      
      # Categorise by Speaker ID
      answer_txt <- full_txt$Content[full_txt$SpeakerID %in% 6:32]
      # Remarks made by LegCo appointment holders (e.g. President)
      misc_txt <- full_txt$Content[full_txt$SpeakerID %in% 1:5]
      question_txt <- full_txt$Content[!full_txt$SpeakerID %in% 1:32]
      
      df$AskingSpeakerID <- list(unique(full_txt$SpeakerID[!full_txt$SpeakerID %in% 1:32]))
      df$AnsweringSpeakerID <- list(answering_speaker_id)
      df$Question[i] <- list(question_txt)
      df$Answer[i] <- list(answer_txt)
      df$Misc[i] <- list(misc_txt)
    }
    
    if (verbose) {
      message(nrow(df), " record(s) match(es) your parameters.")
    }
    
    df
  }
}

#' @rdname answered_questions
#' @export
legco_answered_questions <- answered_questions
