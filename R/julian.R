# Functions to handle Julian calendar dates

#' Julian dates
#'
#' Create a Julian date object.
#'
#' @param year A numeric vector of years
#' @param month A numeric vector of months
#' @param day A numeric vector of days
#' @return A julian vector object
#' @examples
#' julian(2025, 4, 19)
#' @export
julian <- function(
  year = integer(),
  month = integer(),
  day = integer()
) {
  lst <- vec_cast_common(year = year, month = month, day = day, .to = integer())
  lst <- vec_recycle_common(year = lst$year, month = lst$month, day = lst$day)
  check_julian(lst)
  new_rcrd(lst, class = "julian")
}

check_julian <- function(args) {
  year <- args$year
  month <- args$month
  day <- args$day

  if (!all_equal_length(args)) {
    stop("all elements of a date must have the same length")
  } else if (!all_numeric(args)) {
    stop("all elements of a date must be numeric values")
  } else if (!all_integer(args)) {
    stop("all elements of a date must be integer values")
  } else if (any(month < 1 | month > 12)) {
    stop("month must be between 1 and 12")
  } else if (any(day > 30 & month %in% c(4, 6, 9, 11))) {
    stop("day must be between 1 and 30")
  } else if (any(day > 29 & month == 2)) {
    stop("day must be between 1 and 29")
  } else if (any(day > 28 & month == 2 & !julian_leap_year(year))) {
    stop("day must be between 1 and 28")
  } else if (any(day < 1 | day > 31)) {
    stop("day must be between 1 and 31")
  }
}

# Register format method for julian_date
#' @export
format.julian <- function(x, ...) {
  format_date(x)
}

#' @export
vec_ptype_abbr.julian <- function(x, ...) {
  "Jul"
}

#' Convert to a Julian date
#'
#' @param date Vector of dates on some calendar
#' @param ... Additional arguments not currently used
#' @rdname julian
#' @examples
#' as_julian("2016-01-01")
#' as_julian(Sys.Date())
#' tibble::tibble(
#'   x = seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"),
#'   y = as_gregorian(x),
#'   z = as_julian(x)
#' )

#' @export
as_julian <- function(date, ...) {
  UseMethod("as_julian")
}


#' @export
# Convert julian to rd_fixed
as_rd.julian <- function(date, ...) {
  year <- field(date, "year")
  month <- field(date, "month")
  day <- field(date, "day")
  y <- year + (year < 0)
  result <- JULIAN_EPOCH -
    1 + # Days before start of calendar
    365 * (y - 1) + # Ordinary days since epoch
    (y - 1) %/% 4 + # Leap days since epoch
    (367 * month - 362) %/% 12 # Days in prior months this year

  # Adjust for leap years
  adjustment <- (month > 2) * (-2 + julian_leap_year(year))
  rd_fixed(result + adjustment + day)
}

#' @export
# Convert rd_fixed to julian
as_julian.rd_fixed <- function(date, ...) {
  approx <- (4 * (vec_data(date) - JULIAN_EPOCH) + 1464) %/% 1461 # Nominal year
  # Julian (year month day) corresponding to fixed date
  year <- approx - (approx <= 0)

  prior_days <- date - as_rd(julian(year, JANUARY, 1))
  # Correction to simulate a 30-day Feb
  correction <- (date >= as_rd(julian(year, MARCH, 1))) *
    (2 - julian_leap_year(year))
  # Assuming a 30-day Feb
  month <- (12 * (prior_days + correction) + 373) %/% 367
  # Calculate the day by subtraction
  day <- date - as_rd(julian(year, month, 1)) + 1
  julian(year, month, day)
}

#' @export
as_julian.default <- function(date, ...) {
  as_julian(as_rd(date))
}

julian_leap_year <- function(j_year) {
  # True if j_year is a leap year on the Julian calendar
  (j_year > 0) * (j_year %% 4 == 0) + (1 - (j_year > 0)) * (j_year %% 4 == 3)
}


# Arithmetic

#' @export
#' @method vec_arith julian
vec_arith.julian <- function(op, x, y, ...) {
  UseMethod("vec_arith.julian", y)
}
#' @export
#' @method vec_arith.julian julian
vec_arith.julian.julian <- function(op, x, y, ...) {
  vec_arith(op, as_rd(x), as_rd(y))
}
#' @export
#' @method vec_arith.numeric julian
vec_arith.numeric.julian <- function(op, x, y, ...) {
  as_julian(vec_arith(op, as_rd(x), as_rd(y)))
}
#' @export
#' @method vec_arith.julian numeric
vec_arith.julian.numeric <- function(op, x, y, ...) {
  as_julian(vec_arith(op, as_rd(x), as_rd(y)))
}
