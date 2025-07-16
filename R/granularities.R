#' Canonical granularities
#'
#' `granularities()` will return a character vector of canonical granularity names for the
#' relevant calendar. These are the granularities used to define dates on the calendar.
#' `granularity()` will return a vector of numerical values for a given canonical granularity.
#'
#' @param calendar A calcal object defining a calendar.
#' @param date A date vector on some calendar
#' @param granularity A character string indicating the granularity to extract
#' @rdname granularities
#' @return A character vector of granularity names or a vector of numerical values for the specified granularity.
#' @examples
#' granularity_names(cal_iso)
#' granularity_names(cal_gregorian)
#' date_iso <- new_date(year = 2025, week = 23, day = 2, calendar = cal_iso)
#' granularity(date_iso, "week")
#' date_gregorian <- new_date(year = 2025, month = 1, day = 1, calendar = cal_gregorian)
#' granularity(date_gregorian, "month")
#' @seealso [week_of_year] for some non-canonical granularities.
#' @export
granularity_names <- function(calendar) {
  if (inherits(calendar, "rdvec")) {
    calendar <- attributes(calendar)$calendar
  }
  if (!inherits(calendar, "calendar")) {
    stop("`calendar` must be a calendar object", call. = FALSE)
  }
  calendar$granularities
}

#' @rdname granularities
#' @export
granularity <- function(date, granularity) {
  base_granularities(date)[[granularity]]
}
