#==============================================================================
# Gregorian Calendar
#==============================================================================

gregorian_date <- function(year, month, day) {
  c(year, month, day)
}

GREGORIAN_EPOCH <- rd(1)

# Month constants
JANUARY <- 1
FEBRUARY <- 2
MARCH <- 3
APRIL <- 4
MAY <- 5
JUNE <- 6
JULY <- 7
AUGUST <- 8
SEPTEMBER <- 9
OCTOBER <- 10
NOVEMBER <- 11
DECEMBER <- 12

gregorian_leap_year <- function(g_year) {
  (g_year %% 4 == 0) && !(g_year %% 400 %in% c(100, 200, 300))
}

fixed_from_gregorian <- function(g_date) {
  month <- standard_month(g_date)
  day <- standard_day(g_date)
  year <- standard_year(g_date)

  GREGORIAN_EPOCH -
    1 + # Days before start of calendar
    365 * (year - 1) + # Ordinary days since epoch
    (year - 1) %/% 4 - # Julian leap days since epoch...
    (year - 1) %/% 100 + # ...minus century years since epoch...
    (year - 1) %/% 400 + # ...plus years since epoch divisible by 400
    (367 * month - 362) %/% 12 + # Days in prior months this year...
    ifelse(
      month <= 2, # Adjust for February
      0,
      ifelse(gregorian_leap_year(year), -1, -2)
    ) +
    day # Days so far this month
}

gregorian_year_from_fixed <- function(date) {
  d0 <- date - GREGORIAN_EPOCH # Prior days
  n400 <- d0 %/% 146097 # Completed 400-year cycles
  d1 <- d0 %% 146097 # Prior days not in n400
  n100 <- d1 %/% 36524 # 100-year cycles not in n400
  d2 <- d1 %% 36524 # Prior days not in n400 or n100
  n4 <- d2 %/% 1461 # 4-year cycles not in n400 or n100
  d3 <- d2 %% 1461 # Prior days not in n400, n100, or n4
  n1 <- d3 %/% 365 # Years not in n400, n100, or n4
  year <- 400 * n400 + 100 * n100 + 4 * n4 + n1

  if (n100 == 4 || n1 == 4) {
    return(year) # Date is day 366 in a leap year
  } else {
    return(year + 1) # Date is day in (1+ year)
  }
}

gregorian_new_year <- function(g_year) {
  fixed_from_gregorian(gregorian_date(g_year, JANUARY, 1))
}

gregorian_year_end <- function(g_year) {
  fixed_from_gregorian(gregorian_date(g_year, DECEMBER, 31))
}

gregorian_year_range <- function(g_year) {
  interval(gregorian_new_year(g_year), gregorian_new_year(g_year + 1))
}

gregorian_from_fixed <- function(date) {
  year <- gregorian_year_from_fixed(date)
  prior_days <- date - gregorian_new_year(year)

  correction <- ifelse(
    date < fixed_from_gregorian(gregorian_date(year, MARCH, 1)),
    0,
    ifelse(gregorian_leap_year(year), 1, 2)
  )

  month <- (12 * (prior_days + correction) + 373) %/% 367
  day <- 1 + date - fixed_from_gregorian(gregorian_date(year, month, 1))

  gregorian_date(year, month, day)
}

gregorian_date_difference <- function(g_date1, g_date2) {
  fixed_from_gregorian(g_date2) - fixed_from_gregorian(g_date1)
}

day_number <- function(g_date) {
  gregorian_date_difference(
    gregorian_date(standard_year(g_date) - 1, DECEMBER, 31),
    g_date
  )
}

days_remaining <- function(g_date) {
  gregorian_date_difference(
    g_date,
    gregorian_date(standard_year(g_date), DECEMBER, 31)
  )
}

last_day_of_gregorian_month <- function(g_year, g_month) {
  gregorian_date_difference(
    gregorian_date(g_year, g_month, 1),
    gregorian_date(
      ifelse(g_month == 12, g_year + 1, g_year),
      amod(g_month + 1, 12),
      1
    )
  )
}

alt_fixed_from_gregorian <- function(g_date) {
  month <- standard_month(g_date)
  day <- standard_day(g_date)
  year <- standard_year(g_date)

  m_prime <- (month - 3) %% 12
  y_prime <- year - m_prime %/% 10

  GREGORIAN_EPOCH -
    1 -
    306 +
    365 * y_prime +
    y_prime %/% 4 -
    y_prime %/% 100 +
    y_prime %/% 400 +
    (3 * m_prime + 2) %/% 5 +
    30 * m_prime +
    day
}

alt_gregorian_from_fixed <- function(date) {
  y <- gregorian_year_from_fixed(GREGORIAN_EPOCH - 1 + date + 306)
  prior_days <- date - fixed_from_gregorian(gregorian_date(y - 1, MARCH, 1))
  month <- amod((5 * prior_days + 2) %/% 153 + 3, 12)
  year <- y - (month + 9) %/% 12
  day <- 1 + date - fixed_from_gregorian(gregorian_date(year, month, 1))

  gregorian_date(year, month, day)
}
