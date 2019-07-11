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
