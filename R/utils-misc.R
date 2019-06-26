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
  }
  n
}

## Function to search index of search term among multiple columns
search_columns <- function(search_term, split_chr, ...) {
  name_list <- list(...)
  index <- {}
  for (i in 1:length(name_list[[1]])) {
    for (n in 1:length(name_list)) {
      tmp <- unlist(strsplit(name_list[[n]][i], split_chr)) # Split into individual characters/words
      if (sum(tmp %in% search_term) != 0) { # Locate index of string containing the keyword
        index <- c(index, i)
      }
    }
  }
  unique(index)
}
