% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/babylonian.R
\name{babylonian_date}
\alias{babylonian_date}
\alias{as_babylonian}
\title{Babylonian calendar dates}
\usage{
babylonian_date(
  year = integer(),
  month = integer(),
  leap_month = logical(),
  day = integer()
)

as_babylonian(date)
}
\arguments{
\item{year}{Numeric vector of years}

\item{month}{Numeric vector of months}

\item{leap_month}{Logical vector of leap months}

\item{day}{Numeric vector of days}

\item{date}{Vector of dates on some calendar.}
}
\value{
A babylonian vector object
}
\description{
The classical Babylonian calendar was a lunisolar calendar with a fixed 19-year Metonic cycle.
}
\examples{
tibble::tibble(
  gregorian = gregorian_date(2335, 1, 1:2),
  babylonian = as_babylonian(gregorian)
)
babylonian_date(2335, 6, FALSE, 1:2)
}
\seealso{
\link{cal_babylonian}
}
