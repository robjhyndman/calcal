# Julian Calendar
JULIAN_EPOCH <- as_rd(new_gregorian_date(0, 12, 30))@date

julian_date <-
  julian <- S7::new_class(
    "julian",
    properties = list(
      year = class_numeric,
      month = class_numeric,
      day = class_numeric
    ),
    validator = function(self) {
      args <- list(self@year, self@month, self@day)
      if (!all_equal_length(args)) {
        "all elements of a date must have the same length"
      } else if (!all_numeric(args)) {
        "all elements of a date must be numeric values"
      } else if (!all_integer(args)) {
        "all elements of a date must be integer values"
      } else if (any(self@month < 1 | self@month > 12)) {
        "@month must be between 1 and 12"
      } else if (any(self@day > 30 & self@month %in% c(4, 6, 9, 11))) {
        "@day must be between 1 and 30"
      } else if (any(self@day > 29 & self@month == 2)) {
        "@day must be between 1 and 29"
      } else if (
        any(self@day > 28 & self@month == 2 & !julian_leap_year(self@year))
      ) {
        "@day must be between 1 and 28"
      } else if (any(self@day < 1 | self@day > 31)) {
        "@day must be between 1 and 31"
      }
    }
  )

#' Create a new julian_date object
#'
#' @param year The numeric year
#' @param month The numeric month of year
#' @param day The numeric day of month
#' @return A julian_date object
#' @examples
#' new_julian_date(2025, 4, 19)
#' @export
new_julian_date <- function(year, month, day) {
  # Cycle values of length 1
  n <- max(length(year), length(month), length(day))
  if (length(year) == 1) year <- rep(year, n)
  if (length(month) == 1) month <- rep(month, n)
  if (length(day) == 1) day <- rep(day, n)
  julian_date(year, month, day)
}

# Register print method for julian_date
method(print, julian_date) <- function(x, ...) {
  print_date("J", list(x@year, x@month, x@day))
}

#' Convert to julian_date
#'
#' @param date Date on some calendar
#' @param ... Additional arguments
#' @return An julian_date object
#' @examples
#' as_julian("2016-01-01")
#' as_julian(Sys.Date())
#' @export
as_julian <- new_generic("as_julian", "date")

#' @export
# Convert julian_date to rd_fixed
# Method for julian_date objects
method(as_rd, julian_date) <- function(date, ...) {
  y <- date@year + (date@year < 0)
  result <- JULIAN_EPOCH -
    1 + # Days before start of calendar
    365 * (y - 1) + # Ordinary days since epoch
    (y - 1) %/% 4 + # Leap days since epoch
    (367 * date@month - 362) %/% 12 # Days in prior months this year

  # Adjust for leap years
  adjustment <- (date@month > 2) * (-2 + julian_leap_year(date@year))
  new_rd_fixed(result + adjustment + date@day)
}

#' @export
# Convert rd_fixed to julian_date
method(as_julian, rd_fixed) <- function(date, ...) {
  approx <- (4 * (date@date - JULIAN_EPOCH) + 1464) %/% 1461 # Nominal year
  # Julian (year month day) corresponding to fixed date
  year <- approx - (approx <= 0)

  prior_days <- date - as_rd(new_julian_date(year, JANUARY, 1))
  # Correction to simulate a 30-day Feb
  correction <- (date >= as_rd(new_julian_date(year, MARCH, 1))) *
    (2 - julian_leap_year(year))
  # Assuming a 30-day Feb
  month <- (12 * (prior_days + correction) + 373) %/% 367
  # Calculate the day by subtraction
  day <- date - as_rd(new_julian_date(year, month, 1)) + 1
  new_julian_date(year, month, day)
}

method(as_julian, class_any) <- function(date, ...) {
  as_julian(as_rd(date))
}

julian_leap_year <- function(j_year) {
  # True if j_year is a leap year on the Julian calendar
  (j_year > 0) * (j_year %% 4 == 0) + (1 - (j_year > 0)) * (j_year %% 4 == 3)
}
