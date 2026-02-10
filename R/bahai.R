# ==============================================================================
# Baha'i Calendar
# ==============================================================================

BAHAI_EPOCH <- 673222 # as.numeric(gregorian_date(1844, MARCH, 21))
AYYAM_I_HA <- 20 # Signifies intercalary period of 4 or 5 days

validate_bahai <- function(date) {
  if (any(date$cycle < 1 | date$cycle > 19)) {
    stop("Year must be between 1 and 19")
  }
  if (any(date$year < 1 | date$year > 19)) {
    stop("Year must be between 1 and 19")
  }
  if (any(date$month < 1 | date$month > 20)) {
    stop("Month must be between 0 and 20")
  }
  if (any(date$day < 1 | date$day > 19)) {
    stop("Day must be between 1 and 19")
  }
}

format_bahai <- function(x, ...) {
  format_date(
    x,
    month_name = c(
      "Baha",
      "Jalal",
      "Jamal",
      "Azamat",
      "Nur",
      "Rahmat",
      "Kalimat",
      "Kamal",
      "Asma",
      "Izzat",
      "Mashiyyat",
      "Ilm",
      "Qudrat",
      "Qawl",
      "Masail",
      "Sharaf",
      "Sultan",
      "Mulk",
      "Ala",
      "Ayyam-i-Ha"
    )
  )
}

fixed_from_bahai <- function(date) {
  g_year <- 361 *
    (date$major - 1) +
    19 * (date$cycle - 1) +
    date$year -
    1 +
    gregorian_year_from_fixed(BAHAI_EPOCH)

  # Start of year
  out <- as.numeric(gregorian_date(g_year, MARCH, 20))
  # Add months
  case1 <- date$month == AYYAM_I_HA
  case2 <- date$month == 19
  case3 <- !case1 & !case2 & !is.na(out)
  # 18 months have elapsed
  out[case1] <- out[case1] + 342
  # Last month of year. Either a long ayyam-i-ha or an ordinary ayyam-i-ha
  out[case2] <- out[case2] + 346 + gregorian_leap_year(g_year[case2] + 1)
  out[case3] <- out[case3] + (date$month[case3] - 1) * 19
  # Add days
  out + date$day
}

bahai_from_fixed <- function(date) {
  date <- vec_data(date)
  g_year <- gregorian_year_from_fixed(date)
  start <- gregorian_year_from_fixed(BAHAI_EPOCH) # 1844

  # Since start of Baha'i calendar
  years <- g_year -
    start -
    (date <= as.numeric(gregorian_date(g_year, MARCH, 20)))

  major <- 1 + years %/% 361
  cycle <- 1 + (years %% 361) %/% 19
  year <- 1 + years %% 19

  days <- date - vec_data(bahai_date(major, cycle, year, 1, 1))
  case1 <- date >= vec_data(bahai_date(major, cycle, year, 19, 1))
  case2 <- !case1 &
    date >= vec_data(bahai_date(major, cycle, year, AYYAM_I_HA, 1))
  month <- 1 + days %/% 19
  month[case1] <- 19
  month[case2] <- AYYAM_I_HA

  day <- date - vec_data(bahai_date(major, cycle, year, month, 1)) + 1

  list(major = major, cycle = cycle, year = year, month = month, day = day)
}

#' @rdname new_calendar
#' @format NULL
#' @export
cal_bahai <- new_calendar(
  "bahai",
  "Bah",
  c("major", "cycle", "year", "month", "day"),
  validate_bahai,
  format_bahai,
  bahai_from_fixed,
  fixed_from_bahai
)

#' Bahá'í calendar dates
#'
#' The Bahá'í calendar is a solar calendar used in the Bahá'í faith comprising 18 months, with four or five
#' intercalary days each year. The New Year is at the northern Spring equinox,
#' corresponding to 21 March on the Gregorian calendar. Ayyám-i-Há is specified as month 20.
#'
#' @rdname bahai
#' @param major A numeric vector of major periods
#' @param cycle A numeric vector of cycles
#' @param year A numeric vector of years within the cycles
#' @param month A numeric vector of months
#' @param day A numeric vector of days
#' @return A bahai vector object
#' @seealso [cal_bahai], [bahai_new_year]
#' @examples
#' tibble::tibble(
#'   gregorian = gregorian_date(2025, 2, 15) + 0:30,
#'   bahai = as_bahai(gregorian)
#' )
#' bahai_date(1, 10, 11, 3, 5:7)
#' @export
bahai_date <- function(
  major = integer(),
  cycle = integer(),
  year = integer(),
  month = integer(),
  day = integer()
) {
  new_date(
    major = major,
    cycle = cycle,
    year = year,
    month = month,
    day = day,
    calendar = cal_bahai
  )
}

#' @rdname bahai
#' @param date A numeric vector of dates
#' @export
as_bahai <- function(date) {
  as_date(date, calendar = cal_bahai)
}

#' Bahá'í holidays
#'
#' Dates are returned as Gregorian dates
#'
#' @param year The year on the Gregorian calendar
#' @return A vector of dates on the Gregorian calendar
#' @seealso [bahai_date]
#' @examples
#' tibble::tibble(
#'   year = 2025:2030,
#'   new_year = bahai_new_year(year),
#'   naw_ruz = naw_ruz(year),
#'   ridvan = feast_of_ridvan(year),
#'   birth_bab = birth_of_the_bab(year)
#' )
#' @export
bahai_new_year <- function(year) {
  gregorian_date(year, MARCH, 21)
}

bahai_sunset <- function(date) {
  universal_from_standard(
    trunc(as.numeric(date)) +
      as.numeric(sunset(date, BAHAI_LOCATION)) / 24,
    BAHAI_LOCATION
  )
}

astro_bahai_new_year_on_or_before <- function(date) {
  approx <- estimate_prior_solar_longitude(SPRING, bahai_sunset(date))
  next_value(floor(approx) - 1, function(day) {
    solar_longitude(bahai_sunset(day)) <= SPRING + deg(2)
  })
}

fixed_from_astro_bahai <- function(date) {
  years <- 361 * (date$major - 1) + 19 * (date$cycle - 1) + date$year # Years from epoch

  case1 <- date$month == 19
  case2 <- date$month == AYYAM_I_HA
  case3 <- !case1 & !case2
  out <- date$day
  # Last month of year
  out[case1] <- out[case1] +
    astro_bahai_new_year_on_or_before(
      BAHAI_EPOCH + floor(MEAN_TROPICAL_YEAR * (years + 0.5))
    ) -
    20
  # Intercalary month, between 18th & 19th
  out[case2] <-
    astro_bahai_new_year_on_or_before(
      BAHAI_EPOCH + floor(MEAN_TROPICAL_YEAR * (years - 0.5))
    ) +
    341
  # Regular month
  out[case3] <-
    astro_bahai_new_year_on_or_before(
      BAHAI_EPOCH + floor(MEAN_TROPICAL_YEAR * (years - 0.5))
    ) +
    (date$month - 1) * 19 -
    1
  out
}

#' @rdname bahai_new_year
#' @export
naw_ruz <- function(year) {
  astro_bahai_new_year_on_or_before(gregorian_new_year(year + 1)) |>
    as_gregorian()
}

#' @rdname bahai_new_year
#' @export
feast_of_ridvan <- function(year) {
  naw_ruz(year) + 31
}

#' @rdname bahai_new_year
#' @export
birth_of_the_bab <- function(year) {
  ny <- naw_ruz(year)
  set1 <- bahai_sunset(ny)
  m1 <- nth_new_moon(new_moon_at_or_after(set1))
  m8 <- nth_new_moon(new_moon_at_or_after(m1 + 190))
  day <- fixed_from_moment(m8)
  set8 <- bahai_sunset(day)
  as_gregorian(day + 1 + (m8 >= set8))
}

#' @export
day_of_week.bahai <- function(date, ...) {
  dow <- day_of_week_from_fixed(date) + 1
  c(
    "Jamal",
    "Kamal",
    "Fidal",
    "'Idal",
    "Istijlal",
    "Istiqlal",
    "Jalal"
  )[dow]
}
