% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/legco_member_affiliation.R
\docType{data}
\name{legco_member_affiliation}
\alias{legco_member_affiliation}
\title{Political Affliation of LegCo Members}
\format{A list containing 1 dataframe with 10 variables. Each data frame
  represents one ternm, with the `TermID` as the name of the dataframe.
  \describe{ \item{SpeakerID}{The id of the member used in the Hansard
  Database} \item{MemberID}{The id of the member used in the Meeting Schedule
  Database} \item{NameEng}{English full name of the member}
  \item{NameChi}{Chinese full name of the member} \item{TypeEng}{The type of
  consituency that the member belongs to in English} \item{TypeChi}{The type
  of consituency that the member belongs to in Chinese}
  \item{ConstituencyEng}{The consituency that the member belongs to in
  English} \item{ConstituencyChi}{The consituency that the member belongs to
  in Chinese} \item{AffiliationEng}{The declared political affiliation of the
  member in English} \item{AffiliationChi}{The declared political affiliation
  of the member in Chinese} }}
\usage{
legco_member_affiliation
}
\description{
A list of data frames containing the political affiliation and constituency
of each Legco members in each term.
}
\details{
Political affiliation is what the member has declared to LegCo, not necessary
a full list of real-life party membership.
}
\examples{
\dontrun{legco_member_affiliation[["5"]]}

\dontrun{legco_member_affiliation[["5"]]}

}
\keyword{datasets}
