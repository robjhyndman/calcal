% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chinese.R
\name{chinese_date}
\alias{chinese_date}
\alias{as_chinese}
\title{Chinese dates}
\usage{
chinese_date(
  cycle = integer(),
  year = integer(),
  month = integer(),
  leap_month = logical(),
  day = integer()
)

as_chinese(date)
}
\arguments{
\item{cycle}{A numeric vector of cycles}

\item{year}{A numeric vector of years within the cycles}

\item{month}{A numeric vector of months}

\item{leap_month}{A logical vector indicating leap months}

\item{day}{A numeric vector of days}

\item{date}{A numeric vector of dates}
}
\value{
A chinese vector object
}
\description{
The traditional Chinese lunisolar calendar uses a 60-year cycle with 12 months per year.
}
\examples{
chinese <- new_date(
  cycle = 78, year = 42, month = 5, leap_month = FALSE, day = 16:18,
  calendar = cal_chinese
)
chinese
chinese_date(78, 42, 5, FALSE, 16:18)
as_date(chinese, calendar = cal_gregorian)
as_date(Sys.Date(), calendar = cal_chinese)
tibble::tibble(
  gregorian = gregorian_date(2025, 1, 1) + 0:364,
  chinese = as_chinese(gregorian)
)
as_gregorian(chinese_date(78, 41, 12, FALSE, 3:30))
as_chinese(gregorian_date(2025, 1, 1:28))
as_chinese("2016-01-01")
as_chinese(Sys.Date())
}
\seealso{
\link{cal_chinese}, \link{chinese_new_year}
}
