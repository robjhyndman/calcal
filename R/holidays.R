#' US Holidays
#'
#' Functions to return Gregorian dates for (mostly) US holidays and other special days
#'
#' @param year Gregorian year
#'
#' @rdname holidays
#' @examples
#' us_independence_day(2025)
#' us_labor_day(2025)
#' us_memorial_day(2025)
#' us_election_day(2025)
#' us_daylight_saving_start(2025)
#' us_daylight_saving_end(2025)
#' christmas(2025)
#' advent(2025)
#' epiphany(2025)
#'
#' @export
us_independence_day <- function(year) {
  # Fixed date of United States Independence Day in Gregorian year
  gregorian(year, JULY, 4)
}

#' @rdname holidays
#' @export
us_labor_day <- function(year) {
  # Fixed date of United States Labor Day in Gregorian year--the first Monday in September
  as_gregorian(first_kday(MONDAY, gregorian(year, SEPTEMBER, 1)))
}

#' @rdname holidays
#' @export
us_memorial_day <- function(year) {
  # Fixed date of United States Memorial Day in Gregorian year--the last Monday in May
  as_gregorian(last_kday(MONDAY, gregorian(year, MAY, 31)))
}

#' @rdname holidays
#' @export
us_election_day <- function(year) {
  # Fixed date of United States Election Day in Gregorian year--the Tuesday after the first Monday in November
  as_gregorian(first_kday(TUESDAY, gregorian(year, NOVEMBER, 2)))
}

#' @rdname holidays
#' @export
us_daylight_saving_start <- function(year) {
  # Fixed date of the start of United States daylight saving time in Gregorian year--the first Sunday in April
  as_gregorian(first_kday(SUNDAY, gregorian(year, APRIL, 1)))
}

#' @rdname holidays
#' @export
us_daylight_saving_end <- function(year) {
  # Fixed date of the end of United States daylight saving time in Gregorian year--the last Sunday in October
  as_gregorian(last_kday(SUNDAY, gregorian(year, OCTOBER, 31)))
}

#' @rdname holidays
#' @export
christmas <- function(year) {
  # Fixed date of Christmas in Gregorian year
  gregorian(year, DECEMBER, 25)
}

#' @rdname holidays
#' @export
advent <- function(year) {
  # Fixed date of Advent in Gregorian year--the Sunday closest to November 30
  as_gregorian(kday_nearest(
    as_rd(gregorian(year, NOVEMBER, 30)),
    SUNDAY
  ))
}

#' @rdname holidays
#' @export
epiphany <- function(year) {
  # Fixed date of Epiphany in U.S. in Gregorian year--the first Sunday after January 1
  as_gregorian(first_kday(SUNDAY, gregorian(year, JANUARY, 2)))
}

#' @rdname holidays
#' @export
eastern_orthodox_christmas <- function(year) {
  # List of zero or one fixed dates of Eastern Orthodox Christmas in Gregorian year
  julian_in_gregorian(DECEMBER, 25, year)
}
