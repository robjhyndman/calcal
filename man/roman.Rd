% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roman.R
\name{roman_date}
\alias{roman_date}
\alias{as_roman}
\title{Roman calendar dates}
\usage{
roman_date(
  year = integer(),
  month = integer(),
  event = integer(),
  count = integer(),
  leap_day = logical()
)

as_roman(date)
}
\arguments{
\item{year}{A numeric vector of years}

\item{month}{A numeric vector of months}

\item{event}{A numeric vector of events: 1 = Kalends, 2 = Nones, 3 = Ides}

\item{count}{A numeric vector of counts}

\item{leap_day}{A logical vector indicating if day is a leap day}

\item{date}{Vector of dates on some calendar}
}
\value{
A roman vector object
}
\description{
The Roman calendar (as defined here) is the same as the Julian calendar but with different
nomenclature. Rather than use a (year, month, day) triple for each date, it
specifies dates using year, month, event, count.
}
\examples{
roman_date(66, 4, 1, 1, FALSE)
new_date(year = 66, month = 4, event = 1, count = 1, leap_day = FALSE, calendar = cal_roman)
as_roman("2016-01-01")
tibble::tibble(
  x = seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"),
  y = as_roman(x)
)
}
\seealso{
\link{cal_roman}
}
