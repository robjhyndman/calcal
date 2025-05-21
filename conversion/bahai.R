#==============================================================================
# Baha'i Calendar
#==============================================================================

bahai_date <- function(major, cycle, year, month, day) {
  c(major, cycle, year, month, day)
}

bahai_major <- function(date) {
  date[1]
}

bahai_cycle <- function(date) {
  date[2]
}

bahai_year <- function(date) {
  date[3]
}

bahai_month <- function(date) {
  date[4]
}

bahai_day <- function(date) {
  date[5]
}

BAHAI_EPOCH <- fixed_from_gregorian(gregorian_date(1844, MARCH, 21))
AYYAM_I_HA <- 0 # Signifies intercalary period of 4 or 5 days

fixed_from_bahai <- function(b_date) {
  major <- bahai_major(b_date)
  cycle <- bahai_cycle(b_date)
  year <- bahai_year(b_date)
  month <- bahai_month(b_date)
  day <- bahai_day(b_date)

  g_year <- 361 *
    (major - 1) +
    19 * (cycle - 1) +
    year -
    1 +
    gregorian_year_from_fixed(BAHAI_EPOCH)

  fixed_from_gregorian(gregorian_date(g_year, MARCH, 20)) +
    if (month == AYYAM_I_HA) {
      342 # 18 months have elapsed
    } else if (month == 19) {
      # Last month of year
      if (gregorian_leap_year(g_year + 1)) {
        347 # Long ayyam-i-ha
      } else {
        346 # Ordinary ayyam-i-ha
      }
    } else
      {
        19 * (month - 1) # Elapsed months
      } +
        day
}

bahai_from_fixed <- function(date) {
  g_year <- gregorian_year_from_fixed(date)
  start <- gregorian_year_from_fixed(BAHAI_EPOCH) # 1844

  # Since start of Baha'i calendar
  years <- g_year -
    start -
    ifelse(
      date <= fixed_from_gregorian(gregorian_date(g_year, MARCH, 20)),
      1,
      0
    )

  major <- 1 + years %/% 361
  cycle <- 1 + (years %% 361) %/% 19
  year <- 1 + years %% 19

  days <- date - fixed_from_bahai(bahai_date(major, cycle, year, 1, 1)) # Since start of year

  month <- if (
    date >= fixed_from_bahai(bahai_date(major, cycle, year, 19, 1))
  ) {
    19 # Last month of year
  } else if (
    date >= fixed_from_bahai(bahai_date(major, cycle, year, AYYAM_I_HA, 1))
  ) {
    AYYAM_I_HA # Intercalary period
  } else {
    1 + days %/% 19 # Regular month
  }

  day <- date - fixed_from_bahai(bahai_date(major, cycle, year, month, 1)) + 1

  bahai_date(major, cycle, year, month, day)
}

bahai_new_year <- function(g_year) {
  fixed_from_gregorian(gregorian_date(g_year, MARCH, 21))
}

BAHAI_LOCATION <- location(
  angle(35.696111, 0, 0),
  angle(51.423056, 0, 0),
  mt(0),
  hr(3.5)
)

bahai_sunset <- function(date) {
  universal_from_standard(sunset(date, BAHAI_LOCATION), BAHAI_LOCATION)
}

astro_bahai_new_year_on_or_before <- function(date) {
  approx <- estimate_prior_solar_longitude(SPRING, bahai_sunset(date))

  next_value(floor(approx) - 1, function(day) {
    solar_longitude(bahai_sunset(day)) <= SPRING + deg(2)
  })
}

fixed_from_astro_bahai <- function(b_date) {
  major <- bahai_major(b_date)
  cycle <- bahai_cycle(b_date)
  year <- bahai_year(b_date)
  month <- bahai_month(b_date)
  day <- bahai_day(b_date)

  years <- 361 * (major - 1) + 19 * (cycle - 1) + year # Years from epoch

  if (month == 19) {
    # Last month of year
    astro_bahai_new_year_on_or_before(
      BAHAI_EPOCH + floor(MEAN_TROPICAL_YEAR * (years + 0.5))
    ) -
      20 +
      day
  } else if (month == AYYAM_I_HA) {
    # Intercalary month, between 18th & 19th
    astro_bahai_new_year_on_or_before(
      BAHAI_EPOCH + floor(MEAN_TROPICAL_YEAR * (years - 0.5))
    ) +
      341 +
      day
  } else {
    # Regular month
    astro_bahai_new_year_on_or_before(
      BAHAI_EPOCH + floor(MEAN_TROPICAL_YEAR * (years - 0.5))
    ) +
      (month - 1) * 19 +
      day -
      1
  }
}

naw_ruz <- function(g_year) {
  astro_bahai_new_year_on_or_before(gregorian_new_year(g_year + 1))
}

feast_of_ridvan <- function(g_year) {
  naw_ruz(g_year) + 31
}
