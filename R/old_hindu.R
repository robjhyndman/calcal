#==============================================================================
# Old Hindu Calendars
#==============================================================================

HINDU_EPOCH <- -1132959 # as.numeric(julian_date(bce(3102), FEBRUARY, 18))
ARYA_SOLAR_YEAR <- 1577917500 / 4320000
ARYA_SOLAR_MONTH <- ARYA_SOLAR_YEAR / 12
ARYA_LUNAR_MONTH <- 1577917500 / 53433336
ARYA_LUNAR_DAY <- ARYA_LUNAR_MONTH / 30
ARYA_JOVIAN_PERIOD <- 1577917500 / 364224

old_hindu_solar_from_fixed <- function(date) {
  sun <- hindu_day_count(vec_data(date)) + hr(6)
  year <- sun %/% ARYA_SOLAR_YEAR
  month <- 1 + (sun %/% ARYA_SOLAR_MONTH) %% 12
  day <- 1 + floor(sun %% ARYA_SOLAR_MONTH)

  list(year=year, month=month, day=day)
}

fixed_from_old_hindu_solar <- function(date) {
  ceiling(
    HINDU_EPOCH +
      date$year * ARYA_SOLAR_YEAR +
      (date$month - 1) * ARYA_SOLAR_MONTH +
      date$day -
      hr(30)
  )
}

validate_old_hindu_solar <- function(date) {
  if (any(date$month < 1 | date$month > 12)) {
    stop("month must be between 1 and 12")
  } 
  if(any(date$day < 1 | date$day > 31)) {
    stop("day must be between 1 and 31")
  }
}

#' @rdname cal_calendar
#' @format NULL
#' @export
cal_old_hindu_solar <- cal_calendar(
  "old_hindu_solar",
  "Hin",
  c("year", "month", "day"),
  validate_old_hindu_solar,
  format_date,
  old_hindu_solar_from_fixed,
  fixed_from_old_hindu_solar
)


#' Old Hindu Solar dates
#' 
#' @param year A numeric vector of years
#' @param month A numeric vector of months
#' @param day A numeric vector of days
#' @export
old_hindu_solar_date <- function(year, month, day) {
  new_date(year=year, month=month, day=day, calendar = cal_old_hindu_solar)
}

#' @rdname old_hindu_solar_date
#' @param date A date vector on some calendar
#' @examples
#' gregorian_date(2025, 1, 1:31) |>
#'   as_old_hindu_solar()
#' @export
as_old_hindu_solar <- function(date) {
  as_date(date, calendar = cal_old_hindu_solar)
}


hindu_day_count <- function(date) {
  date - HINDU_EPOCH
}

old_hindu_lunar_date <- function(year, month, leap, day) {
  c(year, month, leap, day)
}

old_hindu_lunar_leap_year <- function(l_year) {
  (l_year * ARYA_SOLAR_YEAR - ARYA_SOLAR_MONTH) %% ARYA_LUNAR_MONTH >=
    23902504679 / 1282400064
}

old_hindu_lunar_from_fixed <- function(date) {
  sun <- hindu_day_count(date) + hr(6)
  new_moon <- sun - (sun %% ARYA_LUNAR_MONTH)
  leap <- (ARYA_SOLAR_MONTH - ARYA_LUNAR_MONTH) >=
    (new_moon %% ARYA_SOLAR_MONTH) &&
    (new_moon %% ARYA_SOLAR_MONTH) > 0
  month <- 1 + (ceiling(new_moon / ARYA_SOLAR_MONTH) %% 12)
  day <- 1 + (sun %/% ARYA_LUNAR_DAY) %% 30
  year <- ceiling((new_moon + ARYA_SOLAR_MONTH) / ARYA_SOLAR_YEAR) - 1

  old_hindu_lunar_date(year, month, leap, day)
}


jovian_year <- function(date) {
  amod(27 + hindu_day_count(date) %/% (ARYA_JOVIAN_PERIOD / 12), 60)
}
