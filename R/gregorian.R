# Functions to handle Gregorian dates

gregorian_date <- S7::new_class(
  "gregorian",
  properties = list(
    year = class_numeric,
    month = class_numeric,
    day = class_numeric
  ),
  validator = function(self) {
    if (!all(c(length(self@month), length(self@day)) == length(self@year))) {
      "@year, @month, and @day must have the same length"
    } else if (!is.numeric(self@year)) {
      "@year must be numeric values"
    } else if (!is.numeric(self@month)) {
      "@month must be numeric values"
    } else if (!is.numeric(self@day)) {
      "@day must be numeric values"
    } else if (any(abs(self@year - round(self@year)) > 1e-10)) {
      "@year must be an integer"
    } else if (any(abs(self@month - round(self@month)) > 1e-10)) {
      "@month must be an integer"
    } else if (any(abs(self@day - round(self@day)) > 1e-10)) {
      "@day must be an integer"
    } else if (any(self@month < 1 | self@month > 12)) {
      "@month must be between 1 and 12"
    } else if (any(self@day > 30 & self@month %in% c(4, 6, 9, 11))) {
      "@day must be between 1 and 30"
    } else if (any(self@day > 29 & self@month == 2)) {
      "@day must be between 1 and 29"
    } else if (
      any(self@day > 28 & self@month == 2 & !gregorian_leap_year(self@year))
    ) {
      "@day must be between 1 and 28"
    } else if (any(self@day < 1 | self@day > 31)) {
      "@day must be between 1 and 31"
    }
  }
)

#' Create a new gregorian_date object
#'
#' @param year The numeric year
#' @param month The numeric month of year
#' @param day The numeric day of month
#' @return A gregorian_date object
#' @examples
#' new_gregorian_date(2025, 4, 19)
#' @export
new_gregorian_date <- function(year, month, day) {
  # Cycle values of length 1
  n <- max(length(year), length(month), length(day))
  if (length(year) == 1) year <- rep(year, n)
  if (length(month) == 1) month <- rep(month, n)
  if (length(day) == 1) day <- rep(day, n)
  gregorian_date(year, month, day)
}

# Register print method for gregorian_date
method(print, gregorian_date) <- function(x, ...) {
  paste(
    "G",
    x@year,
    "-",
    sprintf("%.2d", x@month),
    "-",
    sprintf("%.2d", x@day),
    sep = ""
  ) |>
    print()
}

#' Convert to gregorian_date
#'
#' @param date Date on some calendar
#' @param ... Additional arguments
#' @return An gregorian_date object
#' @examples
#' as_gregorian("2016-01-01")
#' as_gregorian(Sys.Date())
#' @export
as_gregorian <- new_generic("as_gregorian", "date")

#' @export
# Convert gregorian_date to rd_fixed
# Method for gregorian_date objects
method(as_rd, gregorian_date) <- function(date, ...) {
  result <- 365 *
    (date@year - 1) + # Ordinary days since epoch
    (date@year - 1) %/% 4 - # Julian leap days since epoch...
    (date@year - 1) %/% 100 + # ...minus century years since epoch...
    (date@year - 1) %/% 400 + # ...plus years since epoch divisible by 400
    (367 * date@month - 362) %/% 12 # Days in prior months this year...
  # Adjust for leap years
  adjustment <- (date@month > 2) * (-2 + gregorian_leap_year(date@year))
  new_rd_fixed(result + adjustment + date@day)
}

#' @export
# Convert rd_fixed to gregorian_date
method(as_gregorian, rd_fixed) <- function(date, ...) {
  # Gregorian (year month day) corresponding to fixed date
  year <- gregorian_year_from_fixed(date)
  prior_days <- date - as_rd(new_gregorian_date(year, 1, 1))
  # Correction to simulate a 30-day Feb
  correction <- (date >= as_rd(new_gregorian_date(year, 3, 1))) *
    (2 - gregorian_leap_year(year))
  # Assuming a 30-day Feb
  month <- (12 * (prior_days + correction) + 373) %/% 367
  # Calculate the day by subtraction
  day <- date - as_rd(new_gregorian_date(year, month, 1)) + 1
  new_gregorian_date(year, month, day)
}

method(as_gregorian, class_any) <- function(date, ...) {
  as_gregorian(as_rd(date))
}

method(as.Date, gregorian_date) <- function(x, ...) {
  as.Date(paste(x@year, x@month, x@day, sep = "-"))
}

gregorian_year_from_fixed <- function(date) {
  # Gregorian year corresponding to the fixed date
  d0 <- date@date - 1 # Prior days
  n400 <- d0 %/% 146097 # Completed 400-year cycles
  d1 <- d0 %% 146097 # Prior days not in n400
  n100 <- d1 %/% 36524 # 100-year cycles not in n400
  d2 <- d1 %% 36524 # Prior days not in n400 or n100
  n4 <- d2 %/% 1461 # 4-year cycles not in n400 or n100
  d3 <- d2 %% 1461 # Prior days not in n400, n100, or n4
  n1 <- d3 %/% 365 # Years not in n400, n100, or n4
  year <- 400 * n400 + 100 * n100 + 4 * n4 + n1

  # leap year adjustment
  leap <- !(n100 == 4 | n1 == 4)
  year + leap
}

gregorian_leap_year <- function(g_year) {
  # True if g_year is a leap year on the Gregorian calendar
  (g_year %% 4 == 0) & !(g_year %% 400 %in% c(100, 200, 300))
}
