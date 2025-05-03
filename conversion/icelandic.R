#==============================================================================
# Icelandic Calendar
#==============================================================================

icelandic_date <- function(year, season, week, weekday) {
  c(year, season, week, weekday)
}

icelandic_year <- function(i_date) {
  i_date[1]
}

icelandic_season <- function(i_date) {
  i_date[2]
}

icelandic_week <- function(i_date) {
  i_date[3]
}

icelandic_weekday <- function(i_date) {
  i_date[4]
}

ICELANDIC_EPOCH <- fixed_from_gregorian(gregorian_date(1, APRIL, 19))
SUMMER <- 1
WINTER <- 2

icelandic_summer <- function(i_year) {
  apr19 <- ICELANDIC_EPOCH + 365 * (i_year - 1) + 
          (i_year %/% 4 - i_year %/% 100 + i_year %/% 400)
  
  kday_on_or_after(THURSDAY, apr19)
}

icelandic_winter <- function(i_year) {
  icelandic_summer(i_year + 1) - 180
}

fixed_from_icelandic <- function(i_date) {
  year <- icelandic_year(i_date)
  season <- icelandic_season(i_date)
  week <- icelandic_week(i_date)
  weekday <- icelandic_weekday(i_date)
  
  start <- if (season == SUMMER) {
    icelandic_summer(year)
  } else {
    icelandic_winter(year)
  }
  
  shift <- if (season == SUMMER) THURSDAY else SATURDAY
  
  start + 7 * (week - 1) + (weekday - shift) %% 7
}

icelandic_from_fixed <- function(date) {
  approx <- (date - ICELANDIC_EPOCH + 369) %/% (146097/400)
  year <- if (date >= icelandic_summer(approx)) approx else approx - 1
  
  season <- if (date < icelandic_winter(year)) SUMMER else WINTER
  
  start <- if (season == SUMMER) {
    icelandic_summer(year)
  } else {
    icelandic_winter(year)
  }
  
  week <- 1 + (date - start) %/% 7
  weekday <- day_of_week_from_fixed(date)
  
  icelandic_date(year, season, week, weekday)
}

icelandic_leap_year <- function(i_year) {
  (icelandic_summer(i_year + 1) - icelandic_summer(i_year)) != 364
}
