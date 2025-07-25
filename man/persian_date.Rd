% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/persian.R
\name{persian_date}
\alias{persian_date}
\alias{apersian_date}
\alias{as_persian}
\alias{as_apersian}
\title{Persian dates}
\usage{
persian_date(year = integer(), month = integer(), day = integer())

apersian_date(year = integer(), month = integer(), day = integer())

as_persian(date)

as_apersian(date)
}
\arguments{
\item{year}{Numeric vector of years}

\item{month}{Numeric vector of months}

\item{day}{Numeric vector of days}

\item{date}{Vector of dates on some calendar}
}
\value{
A persian vector object
}
\description{
The modern Persian calendar was adopted in 1925 in Iran and in 1957 in Afghanistan. An alternative version
of the calendar, using only arithmetic (rather than astronomical) calculations is available as the \code{apersian} calendar.
}
\examples{
gregorian_date(2025,5,1:20) |>
  as_persian()

}
