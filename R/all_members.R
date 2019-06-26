#' All LegCo Members
#'
#' Fetch detailed information of LegCo members.
#'
#' @param speaker_id The Speaker ID, or a vector of IDs, as specified in the
#'   output from the function `legco::speakers()`. Defaults to `NULL`.
#'
#' @param member_id The member ID, or a vector of IDs, as specified in the
#'   output of the function `legco::member()`. Defaults to `NULL`.
#'
#' @param name Search string of member's name. Accepts Chinese or English full
#'   or partial name. If no full match found, return closest match. Defaults to
#'   `NULL`.
#'
#' @param verbose Defaults to `TRUE`.
#'
#' @export
#' 
all_members <- function(speaker_id = NULL, member_id = NULL, name = NULL, verbose = TRUE) {
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
  
  # Drop rows if name search string specified
  if (!is.null(name)) {
    if (grep("[^\001-\177]", name)) { # Detect language of input
      # If Chinese
      index <- which(df$NameChi %in% name)
      index <- c(index, which(df$SurnameChi %in% name))
      index <- c(index, which(df$FirstnameChi %in% name))
      
      if (!length(index)) {
        index <- search_columns(unlist(strsplit(name, ""), "",
                                       df$NameChi,
                                       df$SurnameChi,
                                       df$FirstnameChi))
      }
    } else {
      # If English
      name_tmp <- tolower(name)
      name_tmp <- gsub("-", " ", name_tmp)
      
      index <- which(tolower(df$NameEng) %in% name_tmp)
      index <- c(index, which(tolower(df$SurnameEng) %in% name_tmp))
      tmp <- gsub("-", " ", df$FirstnameEng)
      index <- c(index, which(tolower(tmp) %in% name_tmp))
      index <- c(index, which(tolower(df$EnglishName) %in% name_tmp))
      
      if (!length(index)) {
        index <- search_columns(unlist(strsplit(name_tmp, " "), " ",
                                       df$NameEng,
                                       df$SurnameEng,
                                       gsub("-", " ", df$FirstnameEng),
                                       df$EnglishName))
      }
    }
    
    if (length(index) > 0) {
      df <- df[index, ]
    } else {
      message(paste0("Error: Could not find any matching result for search term \"", name, "\"."))
      df <- NULL
    }
  }
  
  if (verbose) {
    message(paste(nrow(df), "record(s) match(es) your parameters."))
  }
  
  if (!is.null(df)) {
    
    df
  }
}

#' @rdname all_members
#' @export
legco_all_members <- all_members
