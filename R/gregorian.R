# Functions to handle Gregorian dates

#' Gregorian dates
#'
#' Create a Gregorian date object. Dates before the establishment of the Gregorian calendar are computed retrospectively.
#'
#' @param year A numeric vector of years
#' @param month A numeric vector of months
#' @param day A numeric vector of days
#' @return A gregorian vector object
#' @examples
#' gregorian(2025, 4, 19)
#' @export
gregorian <- function(
  year = integer(),
  month = integer(),
  day = integer()
) {
  lst <- vec_cast_common(year = year, month = month, day = day, .to = integer())
  lst <- vec_recycle_common(year = lst$year, month = lst$month, day = lst$day)
  check_gregorian(lst)
  new_rcrd(lst, class = "gregorian")
}

check_gregorian <- function(args) {
  year <- args$year
  month <- args$month
  day <- args$day
  if (any(month < 1 | month > 12, na.rm = TRUE)) {
    stop("month must be between 1 and 12")
  } else if (
    any(day > 30 & month %in% c(APRIL, JUNE, SEPTEMBER, NOVEMBER), na.rm = TRUE)
  ) {
    stop("day must be between 1 and 30")
  } else if (any(day > 29 & month == FEBRUARY, na.rm = TRUE)) {
    stop("day must be between 1 and 29")
  } else if (
    any(day > 28 & month == FEBRUARY & !gregorian_leap_year(year), na.rm = TRUE)
  ) {
    stop("day must be between 1 and 28")
  } else if (any(day < 1 | day > 31, na.rm = TRUE)) {
    stop("day must be between 1 and 31")
  }
}

# Register format method for gregorian date
#' @export
format.gregorian <- function(x, ...) {
  format_date(x)
}

#' @export
vec_ptype_abbr.gregorian <- function(x, ...) {
  "Gre"
}

#' Convert to a Gregorian date
#'
#' @param date Vector of dates on some calendar
#' @param ... Additional arguments not currently used
#' @rdname gregorian
#' @examples
#' as_gregorian("2016-01-01")
#' as_gregorian(Sys.Date())
#' tibble::tibble(
#'   x = seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"),
#'   y = as_gregorian(x)
#' )
#' @export
as_gregorian <- function(date, ...) {
  UseMethod("as_gregorian")
}

#' @export
# Convert gregorian to rd_fixed
as_rd.gregorian <- function(date, ...) {
  year <- field(date, "year")
  month <- field(date, "month")
  day <- field(date, "day")
  result <- GREGORIAN_EPOCH -
    1 + # Days before start of calendar
    365 * (year - 1) + # Ordinary days since epoch
    (year - 1) %/% 4 - # Julian leap days since epoch...
    (year - 1) %/% 100 + # ...minus century years since epoch...
    (year - 1) %/% 400 + # ...plus years since epoch divisible by 400
    (367 * month - 362) %/% 12 # Days in prior months this year...
  # Adjust for leap years
  adjustment <- (month > 2) * (-2 + gregorian_leap_year(year))
  rd_fixed(result + adjustment + day)
}

#' @export
# Convert rd_fixed to gregorian
as_gregorian.rd_fixed <- function(date, ...) {
  # Gregorian (year month day) corresponding to fixed date
  year <- gregorian_year_from_fixed(date)
  prior_days <- date - as_rd(gregorian(year, JANUARY, 1))
  # Correction to simulate a 30-day Feb
  correction <- (date >= as_rd(gregorian(year, MARCH, 1))) *
    (2 - gregorian_leap_year(year))
  # Assuming a 30-day Feb
  month <- (12 * (prior_days + correction) + 373) %/% 367
  # Calculate the day by subtraction
  day <- date - as_rd(gregorian(year, month, 1)) + 1
  gregorian(year, month, day)
}

#' @export
as_gregorian.default <- function(date, ...) {
  as_gregorian(as_rd(date))
}

#' @export
as.Date.gregorian <- function(x, ...) {
  year <- field(x, "year")
  month <- field(x, "month")
  day <- field(x, "day")
  as.Date(paste(year, month, day, sep = "-"))
}

gregorian_year_from_fixed <- function(date) {
  # Gregorian year corresponding to the fixed date
  d0 <- vec_data(date) - GREGORIAN_EPOCH # Prior days
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

#' @title Day of year
#'
#' @description Day number in year or days remaining in year given a Gregorian date
#'
#' @param g_date A Gregorian date object
#' @return A numeric vector of the same length as g_date
#' @examples
#' day_number(gregorian(2025, 5, 2))
#' days_remaining(gregorian(2025, 5, 2))
#'
#' @export
day_number <- function(g_date) {
  # Day number in year of Gregorian date g_date
  as_rd(g_date) - as_rd(gregorian(field(g_date, "year") - 1, DECEMBER, 31))
}

#' @rdname day_number
#' @export
days_remaining <- function(g_date) {
  # Days remaining in year after Gregorian date g_date
  as_rd(gregorian(field(g_date, "year"), DECEMBER, 31)) - as_rd(g_date)
}


# Arithmetic

#' @export
#' @method vec_arith gregorian
vec_arith.gregorian <- function(op, x, y, ...) {
  UseMethod("vec_arith.gregorian", y)
}
#' @export
#' @method vec_arith.gregorian gregorian
vec_arith.gregorian.gregorian <- function(op, x, y, ...) {
  vec_arith(op, as_rd(x), as_rd(y))
}
#' @export
#' @method vec_arith.numeric gregorian
vec_arith.numeric.gregorian <- function(op, x, y, ...) {
  as_gregorian(vec_arith(op, x, as_rd(y)))
}
#' @export
#' @method vec_arith.gregorian numeric
vec_arith.gregorian.numeric <- function(op, x, y, ...) {
  as_gregorian(vec_arith(op, as_rd(x), y))
}
