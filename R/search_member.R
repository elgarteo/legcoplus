#' Search LegCo Member
#'
#' Search LegCo member by SpeakerID, MemberID or/and full or partial English or
#' Chinese name.
#'
#' @param search_term Search string of member's name. Accepts Chinese or English
#'   full or partial name. Defaults to `NULL`.
#'
#' @param exact Whether to look for exact match of the search term. Defaults to
#'   `TRUE`.
#'
#' @param speaker_id The Speaker ID, or a vector of IDs, as specified in the
#'   output from the function `legco::speakers()`. Defaults to `NULL`.
#'
#' @param member_id The member ID, or a vector of IDs, as specified in the
#'   output of the function `legco::member()`. Defaults to `NULL`.
#'
#' @param verbose Defaults to `TRUE`.
#'
#' @export
#' 
search_member <- function(search_term = NULL, speaker_id = NULL, member_id = NULL, 
                          exact = TRUE, verbose = TRUE) {
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
    if (verbose) {
      exist_list <- speaker_id %in% df$SpeakerID
      if (FALSE %in% exist_list) {
        message("Warning: SpeakerID(s) ", paste(speaker_id[!exist_list], collapse = ", "), " do(es) not exist.")
      }
      exist_list <- member_id %in% df$MemberID
      if (FALSE %in% exist_list) {
        message("Warning: MemberID(s) ", paste(member_id[!exist_list], collapse = ", "), " do(es) not exist.")
      }
    }
    
    df <- df[df$SpeakerID %in% speaker_id | df$MemberID %in% member_id, ]
    
    if (!nrow(df)) {
      if (is.null(member_id)) {
        stop("Could not find any matching result for SpeakerID(s) ", paste(speaker_id, collapse = ", "), ".")
      } else if (is.null(speaker_id)) {
        stop("Could not find any matching result for MemberID(s) ", paste(member_id, collapse = ", "), ".")
      } else {
        stop("Could not find any matching result for SpeakerID(s) ", paste(speaker_id, collapse = ", "),
             " and MemberID(s) ", paste(member_id, collapse = ", "), ".")
      }
    }
  }
  
  # Drop rows if name search string specified
  if (!is.null(search_term)) {
    if (grepl("[^\001-\177]", search_term)) { # Detect language of input
      # If Chinese
      if (!exact) {
        search_term <- unlist(strsplit(search_term, ""))
      }
      
      index <- sapply(search_term, function(x) grep(x, df$NameChi))
      index <- c(index, sapply(search_term, function(x) grep(x, df$SurnameChi)))
      index <- c(index, sapply(search_term, function(x) grep(x, df$FirstnameChi)))
      
    } else {
      # If English
      name_tmp <- tolower(search_term)
      fullname_tmp <- tolower(df$NameEng)
      surname_tmp <- tolower(df$SurnameEng)
      firstname_tmp <- tolower(df$FirstnameEng)
      firstname_tmp <- gsub("-", " ", firstname_tmp)
      engname_tmp <- tolower(df$EnglishName)
      
      if (!exact) {
        search_tmp <- tolower(search_term)
        search_tmp <- unlist(strsplit(search_tmp, " "))
      } else {
        search_tmp <- search_term
      }
      
      index <- sapply(search_tmp, function(x) grep(x, fullname_tmp))
      index <- c(index, sapply(search_tmp, function(x) grep(x, surname_tmp)))
      index <- c(index, sapply(search_tmp, function(x) grep(x, firstname_tmp)))
      index <- c(index, sapply(search_tmp, function(x) grep(x, engname_tmp)))
    }
    
    index <- unique(unlist(index))
    
    if (!length(index)) {
      stop("Could not find any matching result for search term \"", search_term, "\".")
    }
    #print(index)
    df <- df[index, ]
    rownames(df) <- 1:nrow(df)
  }
  
  if (verbose) {
    message(nrow(df), " record(s) match(es) your parameters.")
  }
  
  df
}

#' @rdname search_member
#' @export
legco_search_member <- search_member
