# ==============================================================================
# Gregorian Calendar - Day and (US) Holiday Functions
# ==============================================================================

#' US Holidays
#'
#' Functions to return Gregorian dates for US holidays and other special days
#'
#' @param year Gregorian year
#' @return A vector of Gregorian dates corresponding to the US holidays or special days.
#' @rdname us_holidays
#' @examples
#' us_memorial_day(2025)
#' us_independence_day(2025)
#' us_labor_day(2025)
#' us_election_day(2025)
#' us_daylight_saving_start(2025)
#' us_daylight_saving_end(2025)
#' unlucky_fridays(2025)
#'
#' @export
us_memorial_day <- function(year) {
  # Fixed date of United States Memorial Day in Gregorian year--the last Monday in May
  as_gregorian(last_kday(MONDAY, gregorian_date(year, MAY, 31)))
}

#' @rdname us_holidays
#' @export
us_independence_day <- function(year) {
  # Fixed date of United States Independence Day in Gregorian year
  gregorian_date(year, JULY, 4)
}

#' @rdname us_holidays
#' @export
us_labor_day <- function(year) {
  # Fixed date of United States Labor Day in Gregorian year--the first Monday in September
  as_gregorian(first_kday(MONDAY, gregorian_date(year, SEPTEMBER, 1)))
}

#' @rdname us_holidays
#' @export
us_election_day <- function(year) {
  # Fixed date of United States Election Day in Gregorian year--the Tuesday after the first Monday in November
  as_gregorian(first_kday(TUESDAY, gregorian_date(year, NOVEMBER, 2)))
}

#' @rdname us_holidays
#' @export
us_daylight_saving_start <- function(year) {
  # Fixed date of the start of United States daylight saving time in Gregorian year--the first Sunday in April
  as_gregorian(nth_kday(2, SUNDAY, gregorian_date(year, MARCH, 1)))
}

#' @rdname us_holidays
#' @export
us_daylight_saving_end <- function(year) {
  # Fixed date of the end of United States daylight saving time in Gregorian year--the last Sunday in October
  as_gregorian(first_kday(SUNDAY, gregorian_date(year, NOVEMBER, 1)))
}

#' @rdname us_holidays
#' @export
unlucky_fridays <- function(year) {
  ab <- vec_data(gregorian_year_range(year))
  out <- as_gregorian(ab[1]:ab[2])
  out[
    day_of_week(out, numeric = TRUE) == FRIDAY & granularity(out, "day") == 13
  ]
}
