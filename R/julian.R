# ==============================================================================
# Julian Calendar
# ==============================================================================

#' Julian calendar dates
#'
#' Create a Julian date object.
#'
#' @param year A numeric vector of years
#' @param month A numeric vector of months
#' @param day A numeric vector of days
#' @return A julian vector object
#' @examples
#' julian_date(2025, 4, 19:30)
#' @rdname julian_date
#' @export
julian_date <- function(
    year = integer(),
    month = integer(),
    day = integer()) {
  lst <- vec_cast_common(year = year, month = month, day = day, .to = integer())
  lst <- vec_recycle_common(
    year = lst$year,
    month = lst$month,
    day = lst$day,
    .size = max(unlist(lapply(lst, length)))
  )
  check_julian(lst)
  new_rcrd(lst, class = "julian")
}

check_julian <- function(args) {
  year <- args$year
  month <- args$month
  day <- args$day
  if (any(month < 1 | month > 12, na.rm = TRUE)) {
    stop("month must be between 1 and 12")
  } else if (any(day > 30 & month %in% c(4, 6, 9, 11), na.rm = TRUE)) {
    stop("day must be between 1 and 30")
  } else if (any(day > 29 & month == 2, na.rm = TRUE)) {
    stop("day must be between 1 and 29")
  } else if (
    any(day > 28 & month == 2 & !julian_leap_year(year), na.rm = TRUE)
  ) {
    stop("day must be between 1 and 28")
  } else if (any(day < 1 | day > 31, na.rm = TRUE)) {
    stop("day must be between 1 and 31")
  }
}

# Register format method for julian_date
#' @export
format.julian <- function(x, ...) {
  paste(
    sprintf("%.2d", year(x)),
    month.abb[field(x, "month")],
    sprintf("%.2d", field(x, "day")),
    sep = "-"
  )
}

#' @export
as.character.julian <- function(x, ...) {
  format(x)
}

#' @export
vec_ptype_abbr.julian <- function(x, ...) {
  "Jul"
}

#' Convert to a Julian date
#'
#' @param date Vector of dates on some calendar
#' @param ... Additional arguments not currently used
#' @rdname julian_date
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
  y <- year + (year < 0) # No year zero
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

  prior_days <- date - as_rd(julian_date(year, JANUARY, 1))
  # Correction to simulate a 30-day Feb
  correction <- (date >= as_rd(julian_date(year, MARCH, 1))) *
    (2 - julian_leap_year(year))
  # Assuming a 30-day Feb
  month <- (12 * (prior_days + correction) + 373) %/% 367
  # Calculate the day by subtraction
  day <- date - as_rd(julian_date(year, month, 1)) + 1
  julian_date(year, month, day)
}

#' @export
as_julian.default <- function(date, ...) {
  as_julian(as_rd(date))
}


bce <- function(n) {
  -n
}

ce <- function(n) {
  n
}

julian_leap_year <- function(j_year) {
  j_year_mod_4 <- j_year %% 4
  # True if j_year is a leap year on the Julian calendar
  (j_year > 0) * (j_year_mod_4 == 0) + (1 - (j_year > 0)) * (j_year_mod_4 == 3)
}


julian_in_gregorian <- function(j_month, j_day, g_year) {
  # List of the fixed dates of Julian month, day that occur in Gregorian year
  jan1 <- as_rd(gregorian_date(g_year, JANUARY, 1))
  y <- standard_year(as_julian(jan1))
  y_prime <- 1 + (y != -1) * y

  # The possible occurrences in one year are
  date0 <- as_rd(julian_date(y, j_month, j_day))
  date1 <- as_rd(julian_date(y_prime, j_month, j_day))

  out <- mapply(function(d0, d1, year) {
    list_range(c(d0, d1), gregorian_year_range(year))
  }, date0, date1, g_year)
  l <- lapply(out, length)
  out <- out[l > 0]
  if (length(out) > 0) {
    as_gregorian(out)
  } else {
    gregorian_date()
  }
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
  as_julian(vec_arith(op, x, as_rd(y)))
}
#' @export
#' @method vec_arith.julian numeric
vec_arith.julian.numeric <- function(op, x, y, ...) {
  as_julian(vec_arith(op, as_rd(x), y))
}

# Julian Day functions
moment_from_jd <- function(jd) {
  jd + JD_EPOCH
}

jd_from_moment <- function(tee) {
  tee - JD_EPOCH
}

fixed_from_jd <- function(jd) {
  floor(moment_from_jd(jd))
}

jd_from_fixed <- function(date) {
  jd_from_moment(date)
}


fixed_from_mjd <- function(mjd) {
  mjd + MJD_EPOCH
}

mjd_from_fixed <- function(date) {
  date - MJD_EPOCH
}
