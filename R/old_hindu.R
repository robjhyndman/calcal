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

  list(year = year, month = month, day = day)
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
old_hindu_lunar_from_fixed <- function(date) {
  date <- vec_data(date)
  sun <- hindu_day_count(date) + hr(6)
  new_moon <- sun - (sun %% ARYA_LUNAR_MONTH)
  leap <- (ARYA_SOLAR_MONTH - ARYA_LUNAR_MONTH) >=
    (new_moon %% ARYA_SOLAR_MONTH) &
    (new_moon %% ARYA_SOLAR_MONTH) > 0
  month <- 1 + (ceiling(new_moon / ARYA_SOLAR_MONTH) %% 12)
  day <- 1 + (sun %/% ARYA_LUNAR_DAY) %% 30
  year <- ceiling((new_moon + ARYA_SOLAR_MONTH) / ARYA_SOLAR_YEAR) - 1

  list(year = year, month = month, leap_month = leap, day = day)
}

fixed_from_old_hindu_lunar <- function(date) {
  # One solar month before solar new year
  mina <- (12 * date$year - 1) * ARYA_SOLAR_MONTH

  # New moon after mina
  lunar_new_year <- ARYA_LUNAR_MONTH * (1 + mina %/% ARYA_LUNAR_MONTH)

  # Check if there was a leap month this year
  leap_month <- !date$leap &
    ceiling((lunar_new_year - mina) / (ARYA_SOLAR_MONTH - ARYA_LUNAR_MONTH)) <=
      date$month

  # Calculate the final fixed date
  result <- HINDU_EPOCH +
    lunar_new_year +
    ARYA_LUNAR_MONTH * (date$month - !leap_month) +
    (date$day - 1) * ARYA_LUNAR_DAY + # Lunar days
    hr(-6) # Subtract 1 if phase begins before sunrise

  return(ceiling(result))
}

#' @rdname new_calendar
#' @format NULL
#' @export
cal_old_hindu_solar <- new_calendar(
  "old_hindu_solar",
  "OHinS",
  c("year", "month", "day"),
  validate_hindu_solar,
  format_hindu_solar,
  old_hindu_solar_from_fixed,
  fixed_from_old_hindu_solar
)

#' @rdname new_calendar
#' @format NULL
#' @export
cal_old_hindu_lunar <- new_calendar(
  "old_hindu_lunar",
  "OHinL",
  c("year", "month", "leap_month", "day"),
  validate_hindu_lunar,
  format_hindu_lunar,
  old_hindu_lunar_from_fixed,
  fixed_from_old_hindu_lunar
)

#' @rdname hindu_solar_date
#' @export
old_hindu_solar_date <- function(
  year = integer(),
  month = integer(),
  day = integer()
) {
  new_date(
    year = year,
    month = month,
    day = day,
    calendar = cal_old_hindu_solar
  )
}

#' @rdname hindu_solar_date
#' @export
old_hindu_lunar_date <- function(
  year = integer(),
  month = integer(),
  leap_month = logical(),
  day = integer()
) {
  new_date(
    year = year,
    month = month,
    leap_month = leap_month,
    day = day,
    calendar = cal_old_hindu_lunar
  )
}

#' @rdname hindu_solar_date
#' @export
as_old_hindu_solar <- function(date) {
  as_date(date, calendar = cal_old_hindu_solar)
}

#' @rdname hindu_solar_date
#' @export
as_old_hindu_lunar <- function(date) {
  as_date(date, calendar = cal_old_hindu_lunar)
}

hindu_day_count <- function(date) {
  date - HINDU_EPOCH
}

old_hindu_lunar_leap_year <- function(l_year) {
  (l_year * ARYA_SOLAR_YEAR - ARYA_SOLAR_MONTH) %% ARYA_LUNAR_MONTH >=
    23902504679 / 1282400064
}

jovian_year <- function(date) {
  amod(27 + hindu_day_count(date) %/% (ARYA_JOVIAN_PERIOD / 12), 60)
}
