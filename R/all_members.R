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
  df_speaker <- legco::speakers(speaker_id, verbose = verbose)
  df_member <- legco::member(member_id, verbose = verbose)
  df_membership <- legco::membership(member_id = member_id, n = 15000)
  df_committee <- legco::committee()
  
  # Join lists from Speakers() and Member()
  df_member$NameChi <- paste0(df_member$SurnameChi, toupper(df_member$FirstnameChi))
  df <- dplyr::full_join(df_speaker, df_member, by = NameEng)
  df <- data.frame(df$SpeakerID, df$MemberID, df$Type, df$NameChi, df$TitleEng, df$NameEng, 
                   df$SurnameEng, df$FirstnameEng, df$EnglishName, df$HonourableEng, 
                   df$SurnameChi, df$FirstnameChi, df$TitleChi, df$HonourableChi)
  
  # Drop useless rows
  df_membership <- df_membership[df_membership$MemberID %in% df_member$MemberID]
  df_committee <- df_committee[df_committee$CommitteeID %in% df_committee$CommitteeID]
  
  # Attach committee name to membership list with Committee ID
  sapply(df_membership$CommitteeID, function(x) df_committee[df_committee$CommitteeID == x])
  
  
}