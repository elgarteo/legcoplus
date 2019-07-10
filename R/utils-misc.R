## Function to set API limit counter
set_limit <- function() {
  60
}

## Function to pause once API limit is reached
check_limit <- function (n, verbose) {
  n <- n - 1
  if (n == 0) {
    if (verbose) {
      message("API access limit reached. Resuming in 60 seconds...")
    }
    Sys.sleep(60)
    n <- set_limit()
  } else {
    Sys.sleep(2)
  }
  n
}

## Function to search index of search term among multiple columns
search_columns <- function(search_term, split_chr, ...) {
  name_list <- list(...)
  index <- {}
  search_term <- unlist(strsplit(search_term, split_chr))
  for (i in 1:length(name_list[[1]])) {
    for (n in 1:length(name_list)) {
      tmp <- unlist(strsplit(name_list[[n]][i], split_chr)) # Split into individual characters/words
      for (j in 1:length(search_term)) {
        match_boolean <- ifelse(TRUE %in% (tmp %in% search_term[j]), TRUE, FALSE)
        if (match_boolean) {
          break
        }
      }
      if (match_boolean) {
        index <- c(index, i)
        break
      }
    }
  }
  index
}
