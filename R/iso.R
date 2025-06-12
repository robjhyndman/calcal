# ==============================================================================
# ISO Calendar
# ==============================================================================

validate_iso <- function(date) {
  if (any(date$week < 1 | date$week > 53, na.rm = TRUE)) {
    stop("week must be between 1 and 53")
  }
  if (any(date$day < 1 | date$day > 7, na.rm = TRUE)) {
    stop("day must be between 1 and 7")
  }
}

fixed_from_iso <- function(date, ...) {
  nth_kday(date$week, SUNDAY, gregorian_date(date$year - 1, DECEMBER, 28)) +
    date$day
}

iso_from_fixed <- function(date, ...) {
  approx <- gregorian_year_from_fixed(date - 3)
  year <- approx + (date >= iso_date(approx + 1, 1, 1))
  week <- 1 + (date - iso_date(year, 1, 1)) %/% 7
  day <- amod(vec_data(date), 7L)
  list(year = year, week = week, day = day)
}

#' @rdname cal_calendar
#' @format NULL
#' @export
cal_iso <- cal_calendar(
  name = "iso",
  short_name = "ISO",
  granularities = c("year", "week", "day"),
  validate_granularities = validate_iso,
  format = format_date,
  from_rd = iso_from_fixed,
  to_rd = fixed_from_iso
)

#' ISO calendar dates
#'
#' In ISO 8601 date objects, weeks are defined as starting on Mondays. Week 1
#' is the first week with at least 4 days in the year. Equivalently, it is the week
#' containing 4 January. There is no week 0; instead week 1 of a year may begin in
#' the previous calendar year.
#'
#' More flexible week numbering is possible using Gregorian dates with \code{\link{week_of_year}()}.
#'
#' @rdname iso
#' @param year A numeric vector of years
#' @param week A numeric vector of weeks
#' @param day A numeric vector of days
#' @return An iso vector object
#' @seealso [cal_iso], [week_of_year]
#' @examples
#' iso <- new_date(year = 2025, week = 23, day = 2:4, calendar = cal_iso)
#' iso
#' iso_date(2025, 23, 2:4)
#' as_gregorian(iso_date(2025, 23, 2:4))
#' as_iso(gregorian_date(2025, 1, 1:31))
#' as_iso("2016-01-01")
#' as_iso(Sys.Date())
#' tibble::tibble(
#'   x = seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"),
#'   y = as_iso(x)
#' )
#' @export
iso_date <- function(year = integer(), week = integer(), day = integer()) {
  new_date(year = year, week = week, day = day, calendar = cal_iso)
}

#' @rdname iso
#' @param date Vector of dates on some calendar
#' @export
as_iso <- function(date) {
  as_date(date, calendar = cal_iso)
}

iso_long_year <- function(i_year) {
  jan1 <- day_of_week_from_fixed(gregorian_new_year(i_year))
  dec31 <- day_of_week_from_fixed(gregorian_year_end(i_year))
  jan1 == THURSDAY | dec31 == THURSDAY
}
