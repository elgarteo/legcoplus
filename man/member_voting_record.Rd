% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/member_voting_record.R
\name{member_voting_record}
\alias{member_voting_record}
\alias{legco_member_voting_record}
\title{Voting Records of a LegCo Member}
\usage{
member_voting_record(speaker_id = NULL, member_id = NULL,
  committee_id = NULL, meet_id = NULL, from = "1990-01-01",
  to = Sys.Date(), n = 50, verbose = TRUE)

legco_member_voting_record(speaker_id = NULL, member_id = NULL,
  committee_id = NULL, meet_id = NULL, from = "1990-01-01",
  to = Sys.Date(), n = 50, verbose = TRUE)
}
\arguments{
\item{speaker_id}{The Speaker ID, or a vector of IDs. If `NULL`, returns all
LegCo members. Defaults to `NULL`.}

\item{member_id}{The Member ID, or a vector of IDs. If `NULL`, returns all
LegCo members. Defaults to `NULL`.}

\item{committee_id}{The id of a committee, or a vector of IDs. If `NULL`,
returns voting records of all committees. Note that not all committees have
its voting records available. Defaults to `NULL`.}

\item{meet_id}{The id of a meeting. If `NULL`, returns
voting records of all committee meetings. Note that not all committees have
its voting records available. Defaults to `NULL`.}

\item{from}{Only fetch results from hansards of meetings on or after this
date. Accepts character values in `'YYYY-MM-DD'` format, and objects of
class `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or anything else that can be
coerced to a date with `as.Date()`. Defaults to `'1900-01-01'`.}

\item{to}{Only fetch results from hansards of meetings on or before this
date. Accepts character values in `'YYYY-MM-DD'` format, and objects of
class `Date`, `POSIXt`, `POSIXct`, `POSIXlt` or anything else that can be
coerced to a date with `as.Date()`. Defaults to the current system date.}

\item{n}{The number of entry to fetch. Defaults to `50`.}

\item{verbose}{Defaults to `TRUE`.}
}
\description{
Fetch voting records of specific LegCo member(s).
}