% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search_member.R
\name{search_member}
\alias{search_member}
\alias{legco_search_member}
\title{Search LegCo Member}
\usage{
search_member(search_term = NULL, speaker_id = NULL,
  member_id = NULL, exact = TRUE, verbose = TRUE)

legco_search_member(search_term = NULL, speaker_id = NULL,
  member_id = NULL, exact = TRUE, verbose = TRUE)
}
\arguments{
\item{search_term}{Search string of member's name. Accepts Chinese or English
full or partial name. Defaults to `NULL`.}

\item{speaker_id}{The Speaker ID, or a vector of IDs, as specified in the
output from the function `legco::speakers()`. Defaults to `NULL`.}

\item{member_id}{The member ID, or a vector of IDs, as specified in the
output of the function `legco::member()`. Defaults to `NULL`.}

\item{exact}{Whether to look for exact match of the search term. Defaults to
`TRUE`.}

\item{verbose}{Defaults to `TRUE`.}
}
\description{
Search LegCo member by SpeakerID, MemberID or/and full or partial English or
Chinese name.
}
