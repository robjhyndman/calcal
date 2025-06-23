# Babylonian calendar

# Location and epoch constants
BABYLON <- location(deg(32.4794), deg(44.4328), mt(26), hr(3 + 1/2))
BABYLONIAN_EPOCH <- -113502 #vec_data(julian_date(bce(311), APRIL, 3))

fixed_from_babylonian <- function(date) {
   # Elapsed months this year
  month1 <- month - as.numeric(!((leap | ((year %% 19) == 18 & month > 6))))
   # Elapsed months since epoch
  months <- (((year - 1) * 235 + 13) %/% 19) + month1
   # Middle of given month
  midmonth <- BABYLONIAN_EPOCH + round(MEAN_SYNODIC_MONTH * months) + 15

  babylonian_new_month_on_or_before(midmonth) + day - 1
}

babylonian_from_fixed <- function(date) {
  date <- vec_data(date)
  # Most recent new month
  crescent <- babylonian_new_month_on_or_before(date)
  # Elapsed months since epoch
  months <- round((crescent - BABYLONIAN_EPOCH) / MEAN_SYNODIC_MONTH)
  year <- 1 + ((19 * months + 5) %/% 235)
  approx <- BABYLONIAN_EPOCH +
            round(((((year - 1) * 235 + 13) %/% 19) * MEAN_SYNODIC_MONTH))
  new_year <- babylonian_new_month_on_or_before(approx + 15)
  month1 <- 1 + round((crescent - new_year) / 29.5)
  special <- (year %% 19) == 18
  leap <- month1 == 13
  leap[special] <- month1[special] == 7
  month <- month1 - as.numeric((leap | (special & month1 > 6)))
  day <- date - crescent + 1

  list(year=year, month=month, leap=leap, day=day)
}

validate_babylonian <- function(date) {}

#' @rdname cal_calendar
#' @format NULL
#' @export
cal_babylonian <- cal_calendar(
  name = "babylonian",
  "Bab",
  c("year", "month", "leap", "day"),
  validate = validate_babylonian,
  format_date,
  babylonian_from_fixed,
  fixed_from_babylonian
)

#' Babylonian dates
#'
#' @param year Numeric vector of years
#' @param month Numeric vector of months
#' @param leap Logical vector of leap months
#' @param day Numeric vector of days
#' @return A babylonian vector object
#' @seealso [cal_babylonian]
#' @examples
#' babylonian_date(2025, 6, FALSE, 1:10)
#' @export
babylonian_date <- function(year, month, leap, day) {
  new_date(year = year, month = month, leap = leap, day = day,
  calendar = cal_babylonian)
}

#' @rdname babylonian_date
#' @export
as_babylonian <- function(date) {
  as_date(date, calendar = cal_babylonian)
}


# Utility functions
moonlag <- function(date, location) {
  # Time between sunset and moonset on date at location
  # Returns NA if there is no sunset on date
  sun <- as.numeric(sunset(date, location))
  moon <- as.numeric(moonset(date, location))
  moon[is.na(moon)] <- hr(24)  # Arbitrary
  moon - sun
}

babylonian_leap_year_p <- function(b_year) {
  # True if b_year is a leap year on Babylonian calendar
  ((7 * b_year + 13) %% 19) < 7
}

babylonian_criterion <- function(date) {
  # Moonlag criterion for visibility of crescent moon on
  # eve of date in Babylon
  set <- sunset(date - 1, BABYLON)
  set <- date + as.numeric(set)/24
  tee <- universal_from_standard(set, BABYLON)
  phase <- lunar_phase(tee)

  (NEW < phase & phase < FIRST_QUARTER) &
  (new_moon_before(tee) <= (tee - hr(24))) &
  (moonlag(date - 1, BABYLON) > mn(48))
}

babylonian_new_month_on_or_before <- function(date) {
  # Fixed date of start of Babylonian month on or before
  # Babylonian date. Using lag of moonset criterion
  moon <- fixed_from_moment(lunar_phase_at_or_before(NEW, date))
  age <- date - moon
  tau <- moon - 30* as.numeric(age <= 3 & !babylonian_criterion(date))
  while(any(!babylonian_criterion(tau))) {
   j <- which(!babylonian_criterion(tau))
   tau[j] <- tau[j] +1
  }
  tau
}
