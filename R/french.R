#==============================================================================
# French Revolutionary Calendar
#==============================================================================

FRENCH_EPOCH <- 654415 # vec_data(gregorian_date(1792, SEPTEMBER, 22))
PARIS <- location(angle(48, 50, 11), angle(2, 20, 15), mt(27), hr(1))

fixed_from_french <- function(date) {
  new_year <- french_new_year_on_or_before(
    floor(FRENCH_EPOCH + 180 + MEAN_TROPICAL_YEAR * (date$year - 1))
  )
  new_year -
    1 + # Days in prior years
    30 * (date$month - 1) + # Days in prior months
    date$day # Days this month
}

french_from_fixed <- function(date) {
  date <- vec_data(date)
  new_year <- french_new_year_on_or_before(date)
  year <- 1 + round((new_year - FRENCH_EPOCH) / MEAN_TROPICAL_YEAR)
  month <- 1 + (date - new_year) %/% 30
  day <- 1 + (date - new_year) %% 30

  list(year = year, month = month, day = day)
}

fixed_from_arithmetic_french <- function(date) {
  FRENCH_EPOCH -
    1 + # Days before start of calendar
    365 * (date$year - 1) + # Ordinary days in prior years
    (date$year - 1) %/% 4 - # Leap days in prior years
    (date$year - 1) %/% 100 + # Subtract century years
    (date$year - 1) %/% 400 - # Add 400-year cycles
    (date$year - 1) %/% 4000 + # Subtract 4000-year cycles
    30 * (date$month - 1) + # Days in prior months this year
    date$day # Days this month
}

arithmetic_french_from_fixed <- function(date) {
  date <- vec_data(date)
  approx <- 1 + (date - FRENCH_EPOCH + 2) %/% (1460969 / 4000)
  year <- approx - (date < vec_data(afrench_date(approx, 1, 1)))
  month <- 1 +
    (date - vec_data(afrench_date(year, 1, 1))) %/% 30
  day <- 1 + date - vec_data(afrench_date(year, month, 1))

  list(year = year, month = month, day = day)
}

validate_french <- function(date) {
  if (any(date$month < 1 | date$month > 13)) {
    stop("Month must be between 1 and 13")
  }
  if (any(date$day < 1 | date$day > 30)) {
    stop("Day must be between 1 and 30")
  }
  leap_years <- TRUE
  if (any(date$month == 13 & date$day > 6)) {
    stop("13th month can only have 5 or 6 days")
  }
}
validate_afrench <- function(date) {
  if (any(date$month < 1 | date$month > 13)) {
    stop("Month must be between 1 and 13")
  }
  if (any(date$day < 1 | date$day > 30)) {
    stop("Day must be between 1 and 30")
  }
  leap_years <- arithmetic_french_leap_year(date$year)
  if (any(date$month == 13 & date$day > 5 & !leap_years)) {
    stop("13th month can only have 5 days in non-leap years")
  }
  if (any(date$month == 13 & date$day > 6 & leap_years)) {
    stop("13th month can only have 6 days in leap years")
  }
}

format_french <- function(date) {
  format_date(
    date,
    month_name = c(
      "Vend",
      "Brum",
      "Frim",
      "Nivo",
      "Plu",
      "Vent",
      "Germ",
      "Flor",
      "Prai",
      "Mess",
      "Ther",
      "Fruc",
      "xxxx"
    )
  )
}

#' @rdname cal_calendar
#' @format NULL
#' @export
cal_french <- cal_calendar(
  "french",
  "Fre",
  c("year", "month", "day"),
  validate_french,
  format_french,
  french_from_fixed,
  fixed_from_french
)

#' @rdname cal_calendar
#' @format NULL
#' @export
cal_afrench <- cal_calendar(
  "afrench",
  "AFre",
  c("year", "month", "day"),
  validate_afrench,
  format_french,
  arithmetic_french_from_fixed,
  fixed_from_arithmetic_french
)

#' French Revolutionary Dates
#'
#' There are two versions of the French Revolutionary Calendar. The original
#' version, used from 1793, was kept in sync with the solar year by setting the first day of
#' Vendemiaire to the autumnal equinox. The second version, proposed in 1795, was a simpler
#' arithmetic calendar, but was never used. We distinguish the two by using "afrench"
#' (for Arithmetic French) for the second form.
#'
#' @param year year
#' @param month month
#' @param day day
#' @examples
#' french_date(1, 1, 1:15) |>
#'   as_gregorian()
#' french_date(1, 1, 1:15) |>
#'   day_of_week()
#' @export
french_date <- function(year, month, day) {
  new_date(year = year, month = month, day = day, calendar = cal_french)
}

#' @rdname french_date
#' @export
afrench_date <- function(year, month, day) {
  new_date(year = year, month = month, day = day, calendar = cal_afrench)
}

#' @rdname french_date
#' @param date A vector of dates on some calendar
#' @export
as_french <- function(date) {
  as_date(date, cal_french)
}

#' @rdname french_date
#' @param date A vector of dates on some calendar
#' @export
as_afrench <- function(date) {
  as_date(date, cal_afrench)
}

midnight_in_paris <- function(date) {
  midnight(date + 1, PARIS)
}

french_new_year_on_or_before <- function(date) {
  approx <- estimate_prior_solar_longitude(AUTUMN, midnight_in_paris(date))
  out <- floor(approx) - 1
  while (any(AUTUMN > solar_longitude(midnight_in_paris(out)))) {
    j <- which(AUTUMN > solar_longitude(midnight_in_paris(out)))
    out[j] <- out[j] + 1
  }
  return(out)
}

french_leap_year <- function(f_year) {
  (french_date(f_year + 1, 1, 1) -
    french_date(f_year, 1, 1)) >
    365
}

arithmetic_french_leap_year <- function(f_year) {
  (f_year %% 4 == 0) &
    !(f_year %% 400 %in% c(100, 200, 300)) &
    !(f_year %% 4000 == 0)
}

#' @export
day_of_week.french <- function(date, ...) {
  dom <- granularity(date, "day")
  dow <- amod(dom, 10)
  month <- granularity(date, "month")
  dow1 <- c(
    "Primidi",
    "Duodi",
    "Tridi",
    "Quartidi",
    "Quintidi",
    "Sextidi",
    "Septidi",
    "Octidi",
    "Nonidi",
    "Decadi"
  )[dow]
  dow2 <- c(
    "Vertu",
    "Genie",
    "Travail",
    "Opinion",
    "Recompense",
    "Revolution"
  )[dow]
  dow1[month == 13] <- dow2[month == 13]
  dow1
}
