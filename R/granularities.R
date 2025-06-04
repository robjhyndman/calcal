# Find and extract canonical granularities from a calendar object

#' Canonical granularities for a calendar
#'
#' @param dates A vector of calendar dates
#' @rdname granularities
#' @examples
#' granularities(iso_date(2025, 23, 2:4))
#' granularities(gregorian_date(2025, 4, 19:30))
#' granularities(as_roman(Sys.Date()))
#' @export
granularities <- function(dates) {
  vctrs::fields(dates)
}

#' Extract a granularity from a calendar
#'
#' @param dates A vector of calendar dates
#' @param granularity A character string indicating the granularity to extract
#' @rdname granularities
#' @examples
#' granularity(iso_date(2025, 23, 2:4), "week")
#' granularity(gregorian_date(2025, 4, 19:21), "month")
#' granularity(as_roman(Sys.Date()), "event")
#' @export
granularity <- function(dates, granularity) {
  vctrs::field(dates, granularity)
}
