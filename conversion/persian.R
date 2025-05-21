#==============================================================================
# Persian Calendar
#==============================================================================

persian_date <- function(year, month, day) {
  c(year, month, day)
}

PERSIAN_EPOCH <- fixed_from_julian(julian_date(ce(622), MARCH, 19))

midday_in_tehran <- function(date) {
  midday(date, TEHRAN)
}

persian_new_year_on_or_before <- function(date) {
  approx <- estimate_prior_solar_longitude(SPRING, midday_in_tehran(date))

  next_value(floor(approx) - 1, function(day) {
    solar_longitude(midday_in_tehran(day)) <= SPRING + deg(2)
  })
}

fixed_from_persian <- function(p_date) {
  month <- standard_month(p_date)
  day <- standard_day(p_date)
  year <- standard_year(p_date)

  new_year <- persian_new_year_on_or_before(
    PERSIAN_EPOCH +
      180 +
      floor(MEAN_TROPICAL_YEAR * ifelse(year > 0, year - 1, year))
  )

  new_year -
    1 + # Days in prior years
    ifelse(
      month <= 7, # Days in prior months this year
      (month - 1) * 31, # First 7 months have 31 days
      (month - 1) * 30 + 6
    ) + # Remaining months have 30 days
    day # Days so far this month
}

persian_from_fixed <- function(date) {
  new_year <- persian_new_year_on_or_before(date)
  y <- round((new_year - PERSIAN_EPOCH) / MEAN_TROPICAL_YEAR)
  year <- if (y > 0) y else y - 1 # No year zero
  day_of_year <- 1 + date - fixed_from_persian(persian_date(year, 1, 1))
  month <- if (day_of_year <= 186) {
    ceiling(day_of_year / 31)
  } else {
    ceiling((day_of_year - 6) / 30)
  }
  day <- date - (fixed_from_persian(persian_date(year, month, 1)) - 1)

  persian_date(year, month, day)
}

arithmetic_persian_leap_year <- function(p_year) {
  y <- if (p_year > 0) p_year - 474 else p_year - 473 # Years since start of 2820-year cycles
  year <- (y %% 2820) + 474 # Equivalent year in the range 474..3263

  ((year + 38) * 31) %% 128 < 31
}

fixed_from_arithmetic_persian <- function(p_date) {
  day <- standard_day(p_date)
  month <- standard_month(p_date)
  p_year <- standard_year(p_date)

  y <- if (p_year > 0) p_year - 474 else p_year - 473 # Years since start of 2820-year cycle
  year <- (y %% 2820) + 474 # Equivalent year in the range 474..3263

  PERSIAN_EPOCH -
    1 + # Days before epoch
    1029983 * (y %/% 2820) + # Days in 2820-year cycles before Persian year 474
    365 * (year - 1) + # Nonleap days in prior years this 2820-year cycle
    ((31 * year - 5) %/% 128) + # Leap days in prior years this 2820-year cycle
    ifelse(
      month <= 7, # Days in prior months this year
      (month - 1) * 31, # First 7 months have 31 days
      (month - 1) * 30 + 6
    ) + # Remaining months have 30 days
    day # Days so far this month
}

arithmetic_persian_from_fixed <- function(date) {
  year <- arithmetic_persian_year_from_fixed(date)
  day_of_year <- 1 +
    date -
    fixed_from_arithmetic_persian(persian_date(year, 1, 1))
  month <- if (day_of_year <= 186) {
    ceiling(day_of_year / 31)
  } else {
    ceiling((day_of_year - 6) / 30)
  }
  day <- date -
    (fixed_from_arithmetic_persian(persian_date(year, month, 1)) - 1)

  persian_date(year, month, day)
}

arithmetic_persian_year_from_fixed <- function(date) {
  # Prior days since start of 2820-year cycle beginning in Persian year 474
  d0 <- date - fixed_from_arithmetic_persian(persian_date(475, 1, 1))
  n2820 <- d0 %/% 1029983 # Completed prior 2820-year cycles
  d1 <- d0 %% 1029983 # Prior days not in n2820

  # Years since start of last 2820-year cycle
  y2820 <- if (d1 == 1029982) {
    2820 # Last day of 2820-year cycle
  } else {
    (128 * d1 + 46878) %/% 46751 # Otherwise use cycle of years formula
  }

  # Years since Persian epoch
  year <- 474 + 2820 * n2820 + y2820

  if (year > 0) year else year - 1 # No year zero
}

nowruz <- function(g_year) {
  persian_year <- 1 + (g_year - gregorian_year_from_fixed(PERSIAN_EPOCH))
  y <- if (persian_year <= 0) persian_year - 1 else persian_year # No Persian year 0

  fixed_from_persian(persian_date(y, 1, 1))
}
