% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aa_calendar.R
\name{as_date}
\alias{as_date}
\alias{new_date}
\title{Create a new date vector or convert a date vector to a new calendar}
\usage{
as_date(date, calendar)

new_date(..., calendar)
}
\arguments{
\item{date}{Date vector on some calendar}

\item{calendar}{Target calendar of class "calcal"}

\item{...}{Named arguments denoting the granularities required for \code{calendar}.}
}
\description{
Create a new date vector or convert a date vector to a new calendar
}
\examples{
april25 <- new_date(year = 2025, month = 4, day = 1:30, calendar = cal_gregorian)
as_date(april25, calendar = cal_iso)
}
