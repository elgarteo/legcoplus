#' All LegCo Members
#'
#' Fetch detailed information of LegCo members.
#'
#' @param speaker_id The Speaker ID, or a vector of IDs, as specified in the
#'   output from the function `legco::speakers()`. Defaults to `NULL`.
#'
#' @param member_id The member ID, or a vector of IDs, as specified in the output of the
#'   function `legco::member()`. Defaults to `NULL`.
#'
#' @param verbose Defaults to `TRUE`.
#'
#' @export
#' 
all_members <- function(speaker_id = NULL, member_id = NULL, verbose = TRUE) {
  df_speaker <- legco::speakers(verbose = verbose)
  df_member <- legco::member(verbose = verbose)
  df_term <- legco::member_term(verbose = verbose)
  
  df_member$NameChi <- paste0(df_member$SurnameChi, toupper(df_member$FirstnameChi))
  df_member <- dplyr::full_join(df_member, df_term, by = "MemberID")
  df <- dplyr::full_join(df_speaker, df_member, by = "NameChi")
  df <- df[c("SpeakerID", "MemberID", "Type", "NameChi",  "NameEng", "TermID", "TitleEng", 
             "SurnameEng", "FirstnameEng", "EnglishName", "HonourableEng", "SurnameChi",
             "FirstnameChi", "TitleChi", "HonourableChi")]
  
  # Drop rows if SpeakerID or MemberID specified
  if (!is.null(speaker_id) | !is.null(member_id)) {
    df <- df[df$SpeakerID %in% speaker_id | df$MemberID %in% member_id, ]
  }
  
  df
  
}

#' @rdname all_members
#' @export
legco_all_members <- all_members
