#' Find and extract canonical granularities from a calendar object
#'
#' `granularities()` will return a character vector of granularity names for the
#' relevant calendar.
#' `granularity()` will return a vector of numerical values for a given granularity.
#'
#' @param dates A vector of calendar dates
#' @rdname granularities
#' @examples
#' granularities(iso_date(2025, 23, 2))
#' granularities(gregorian_date(2025, 4, 19))
#' granularities(as_roman(Sys.Date()))
#' @export
granularities <- function(dates) {
  vctrs::fields(dates)
}

#' @param granularity A character string indicating the granularity to extract
#' @rdname granularities
#' @examples
#' granularity(iso_date(2025, 23, 2), "week")
#' granularity(gregorian_date(2025, 4, 19), "month")
#' granularity(as_roman(Sys.Date()), "count")
#' @export
granularity <- function(dates, granularity) {
  vctrs::field(dates, granularity)
}
