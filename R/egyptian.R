#==============================================================================
# Egyptian/Armenian Calendars
#==============================================================================

EGYPTIAN_EPOCH <- -272787 # fixed_from_jd(1448638) # February 26, 747 BCE (Julian)
ARMENIAN_EPOCH <- 201443 # julian_date(552,7,11) |> as.numeric() July 11, 552 CE (Julian)

fixed_from_egyptian <- function(date) {
  EGYPTIAN_EPOCH -
    1 + # Days before start of calendar
    365 * (date$year - 1) + # Days in prior years
    30 * (date$month - 1) + # Days in prior months this year
    date$day # Days so far this month
}

egyptian_from_fixed <- function(date) {
  days <- vec_data(date) - EGYPTIAN_EPOCH # Elapsed days since epoch
  year <- 1 + days %/% 365 # Year since epoch
  month <- 1 + (days %% 365) %/% 30 # Calculate month by division
  day <- days - # Calculate day by subtraction
    365 * (year - 1) -
    30 * (month - 1) +
    1

  list(year = year, month = month, day = day)
}

fixed_from_armenian <- function(date) {
  ARMENIAN_EPOCH + (fixed_from_egyptian(date) - EGYPTIAN_EPOCH)
}

armenian_from_fixed <- function(date) {
  egyptian_from_fixed(date + (EGYPTIAN_EPOCH - ARMENIAN_EPOCH))
}

validate_egyptian <- function(date) {
  if (any(date$month < 1 | date$month > 13)) {
    stop("month must be between 1 and 13")
  }
  if (any(date$day < 1 | date$day > 30)) {
    stop("day must be between 1 and 30")
  }
  if (any(date$month == 13 & date$day > 5)) {
    stop("13th month can only have 5 days")
  }
}

format_egyptian <- function(date) {
  format_date(
    date,
    month_name = c(
      "Thot",
      "Phao",
      "Athy",
      "Choi",
      "Tybi",
      "Mech",
      "Pham",
      "Phar",
      "Pach",
      "Payn",
      "Epip",
      "Meso",
      "xxxx"
    )
  )
}

format_armenian <- function(date) {
  format_date(
    date,
    month_name = c(
      "Nawa",
      "Hori",
      "Sahm",
      "Tre",
      "Kalo",
      "Arac",
      "Mehe",
      "Areg",
      "Ahek",
      "Mare",
      "Marg",
      "Hrot",
      "xxxx"
    )
  )
}

#' @rdname new_calendar
#' @format NULL
#' @export
cal_egyptian <- new_calendar(
  name = "egyptian",
  short_name = "Egy",
  granularities = c("year", "month", "day"),
  validate_granularities = validate_egyptian,
  format = format_egyptian,
  from_rd = egyptian_from_fixed,
  to_rd = fixed_from_egyptian
)

#' @rdname new_calendar
#' @format NULL
#' @export
cal_armenian <- new_calendar(
  name = "armenian",
  short_name = "Arm",
  granularities = c("year", "month", "day"),
  validate_granularities = validate_egyptian,
  format = format_armenian,
  from_rd = armenian_from_fixed,
  to_rd = fixed_from_armenian
)

#' Egyptian and Armenian calendar dates
#'
#' The ancient Egyptian calendar is a 365-day solar calendar with 12 months of 30 days each, plus a 13th month of 5 days.
#' The Armenian calendar is similar but has a different epoch and month names.
#'
#' @rdname egyptian
#' @param year Numeric vector of years
#' @param month Numeric vector of months
#' @param day Numeric vector of days
#' @return An egyptian or armenian vector object
#' @examples
#' tibble::tibble(
#'   gregorian = gregorian_date(2025, 5, 1:10),
#'   egyptian = as_egyptian(gregorian),
#'   armenian = as_armenian(gregorian)
#' )
#' @export
egyptian_date <- function(
  year = integer(),
  month = integer(),
  day = integer()
) {
  new_date(year = year, month = month, day = day, calendar = cal_egyptian)
}

#' @rdname egyptian
#' @export
armenian_date <- function(
  year = integer(),
  month = integer(),
  day = integer()
) {
  new_date(year = year, month = month, day = day, calendar = cal_armenian)
}

#' @rdname egyptian
#' @param date Vector of dates on some calendar
#' @export
as_egyptian <- function(date) {
  as_date(date, calendar = cal_egyptian)
}

#' @rdname egyptian
#' @export
as_armenian <- function(date) {
  as_date(date, calendar = cal_armenian)
}
