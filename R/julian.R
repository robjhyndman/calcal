# ==============================================================================
# Julian Calendar
# ==============================================================================

JULIAN_EPOCH <- -1 # vec_data(gregorian_date(0, DECEMBER, 30))
JD_EPOCH <- -1721424.5
MJD_EPOCH <- 678576

validate_julian <- function(date) {
  if (any(date$month < 1 | date$month > 12, na.rm = TRUE)) {
    stop("month must be between 1 and 12")
  } else if (
    any(date$day > 30 & date$month %in% c(4, 6, 9, 11), na.rm = TRUE)
  ) {
    stop("day must be between 1 and 30")
  } else if (any(date$day > 29 & date$month == 2, na.rm = TRUE)) {
    stop("day must be between 1 and 29")
  } else if (
    any(
      date$day > 28 & date$month == 2 & !julian_leap_year(date$year),
      na.rm = TRUE
    )
  ) {
    stop("day must be between 1 and 28")
  } else if (any(date$day < 1 | date$day > 31, na.rm = TRUE)) {
    stop("day must be between 1 and 31")
  }
}

# Register format method for julian_date
format_julian <- function(x, ...) {
  lst <- base_granularities(x)
  paste(
    sprintf("%.2d", lst$year),
    month.abb[lst$month],
    sprintf("%.2d", lst$day),
    sep = "-"
  )
}

# Convert julian to RD
fixed_from_julian <- function(date, ...) {
  y <- date$year + (date$year < 0) # No year zero
  result <- JULIAN_EPOCH -
    1 + # Days before start of calendar
    365 * (y - 1) + # Ordinary days since epoch
    (y - 1) %/% 4 + # Leap days since epoch
    (367 * date$month - 362) %/% 12 # Days in prior months this year

  # Adjust for leap years
  adjustment <- (date$month > 2) * (-2 + julian_leap_year(date$year))
  result + adjustment + date$day
}

# Convert RD to julian
julian_from_fixed <- function(date, ...) {
  approx <- (4 * (vec_data(date) - JULIAN_EPOCH) + 1464) %/% 1461 # Nominal year
  # Julian (year month day) corresponding to fixed date
  year <- approx - (approx <= 0)

  prior_days <- date - julian_date(year, JANUARY, 1)
  # Correction to simulate a 30-day Feb
  correction <- (date >= julian_date(year, MARCH, 1)) *
    (2 - julian_leap_year(year))
  # Assuming a 30-day Feb
  month <- (12 * (prior_days + correction) + 373) %/% 367
  # Calculate the day by subtraction
  day <- date - julian_date(year, month, 1) + 1
  list(year = year, month = month, day = day)
}

#' @rdname cal_calendar
#' @format NULL
#' @export
cal_julian <- cal_calendar(
  name = "julian",
  short_name = "Jul",
  granularities = c("year", "month", "day"),
  validate_granularities = validate_julian,
  format = format_julian,
  from_rd = julian_from_fixed,
  to_rd = fixed_from_julian
)

#' Julian dates
#'
#' The Julian calendar is the calendar used by the Roman Empire, and
#' takes its name from Julius Caesar, who introduced it in 46 BC. It
#' is still used as a religious calendar in parts of the Eastern
#' Orthodox Church.
#'
#' @rdname julian
#' @param year A numeric vector of years
#' @param month A numeric vector of months
#' @param day A numeric vector of days
#' @seealso [cal_julian]
#' @return A julian vector object
#' @examples
#' as_date("2016-01-01", calendar = cal_julian)
#' as_date(Sys.Date(), calendar = cal_julian)
#' tibble::tibble(
#'   x = seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"),
#'   y = as_date(x, calendar = cal_gregorian),
#'   z = as_date(x, calendar = cal_julian)
#' )
#' new_date(year = 2025, month = 4, day = 19:30, calendar = cal_julian)
#' julian_date(2025, 4, 19:30)
#' @export
julian_date <- function(year = integer(), month = integer(), day = integer()) {
  new_date(year = year, month = month, day = day, calendar = cal_julian)
}

#' @rdname julian
#' @param date Vector of dates on some calendar
#' @examples
#' as_julian("2016-01-01")
#' as_julian(Sys.Date())
#' tibble::tibble(
#'   x = seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"),
#'   y = as_julian(x)
#' )
#' @export
as_julian <- function(date) {
  as_date(date, calendar = cal_julian)
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
  jan1 <- gregorian_date(g_year, JANUARY, 1)
  y <- granularity(as_julian(jan1), "year")
  y_prime <- 1 + (y != -1) * y

  # The possible occurrences in one year are
  date0 <- julian_date(y, j_month, j_day)
  date1 <- julian_date(y_prime, j_month, j_day)
  dates2_in_gregorian(g_year, date0, date1)
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
