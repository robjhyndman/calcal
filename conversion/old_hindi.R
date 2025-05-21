#==============================================================================
# Old Hindu Calendars
#==============================================================================

old_hindu_lunar_date <- function(year, month, leap, day) {
  c(year, month, leap, day)
}

old_hindu_lunar_month <- function(date) {
  date[2]
}

old_hindu_lunar_leap <- function(date) {
  date[3]
}

old_hindu_lunar_day <- function(date) {
  date[4]
}

old_hindu_lunar_year <- function(date) {
  date[1]
}

hindu_solar_date <- function(year, month, day) {
  c(year, month, day)
}

HINDU_EPOCH <- fixed_from_julian(julian_date(bce(3102), FEBRUARY, 18))

hindu_day_count <- function(date) {
  date - HINDU_EPOCH
}

ARYA_SOLAR_YEAR <- 1577917500 / 4320000
ARYA_SOLAR_MONTH <- ARYA_SOLAR_YEAR / 12

old_hindu_solar_from_fixed <- function(date) {
  sun <- hindu_day_count(date) + hr(6)
  year <- sun %/% ARYA_SOLAR_YEAR
  month <- 1 + (sun %/% ARYA_SOLAR_MONTH) %% 12
  day <- 1 + floor(sun %% ARYA_SOLAR_MONTH)

  hindu_solar_date(year, month, day)
}

fixed_from_old_hindu_solar <- function(s_date) {
  month <- standard_month(s_date)
  day <- standard_day(s_date)
  year <- standard_year(s_date)

  ceiling(
    HINDU_EPOCH +
      year * ARYA_SOLAR_YEAR +
      (month - 1) * ARYA_SOLAR_MONTH +
      day -
      hr(30)
  )
}

ARYA_LUNAR_MONTH <- 1577917500 / 53433336
ARYA_LUNAR_DAY <- ARYA_LUNAR_MONTH / 30

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

ARYA_JOVIAN_PERIOD <- 1577917500 / 364224

jovian_year <- function(date) {
  amod(27 + hindu_day_count(date) %/% (ARYA_JOVIAN_PERIOD / 12), 60)
}
