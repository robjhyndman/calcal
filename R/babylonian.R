# Babylonian calendar

# Location and epoch constants
BABYLON <- location(deg(32.4794), deg(44.4328), mt(26), 3.5)
BABYLONIAN_EPOCH <- -113502 #vec_data(julian_date(bce(311), APRIL, 3))

fixed_from_babylonian <- function(date) {
  # Elapsed months this year
  month1 <- date$month -
    as.numeric(!((date$leap | ((date$year %% 19) == 18 & date$month > 6))))
  # Elapsed months since epoch
  months <- (((date$year - 1) * 235 + 13) %/% 19) + month1
  # Middle of given month
  midmonth <- BABYLONIAN_EPOCH + round(MEAN_SYNODIC_MONTH * months) + 15
  bnm <- rep(NA_integer_, length(midmonth))
  bnm[!is.na(midmonth)] <-
    babylonian_new_month_on_or_before(midmonth[!is.na(midmonth)])
  bnm + date$day - 1
}

babylonian_from_fixed <- function(date) {
  date <- vec_data(date)
  miss <- is.na(date)
  # Most recent new month
  crescent <- new_year <- rep(NA_real_, length(date))
  crescent[!miss] <- babylonian_new_month_on_or_before(date[!miss])
  # Elapsed months since epoch
  months <- round((crescent - BABYLONIAN_EPOCH) / MEAN_SYNODIC_MONTH)
  year <- 1 + ((19 * months + 5) %/% 235)
  approx <- BABYLONIAN_EPOCH +
    round(((((year - 1) * 235 + 13) %/% 19) * MEAN_SYNODIC_MONTH))
  new_year[!miss] <- babylonian_new_month_on_or_before(approx[!miss] + 15)
  month1 <- 1 + round((crescent - new_year) / 29.5)
  special <- (year %% 19) == 18 & !miss
  leap <- month1 == 13
  leap[special] <- month1[special] == 7
  month <- month1 - as.numeric((leap | (special & month1 > 6)))
  day <- date - crescent + 1

  list(year = year, month = month, leap_month = leap, day = day)
}

validate_babylonian <- function(date) {
  if (any(date$month < 1 | date$month > 12)) {
    stop("month must be between 1 and 12")
  }
  if (any(date$day < 1 | date$day > 30)) {
    stop("day must be between 1 and 30")
  }
}

format_babylonian <- function(x, ...) {
  format_date(
    x,
    month_name = c(
      "Nisa",
      "Ayar",
      "Sima",
      "Du'uz",
      "Abu",
      "Ulul",
      "Tash",
      "Arak",
      "Kisl",
      "Tebe",
      "Shab",
      "Adar"
    )
  )
}

#' @rdname new_calendar
#' @format NULL
#' @export
cal_babylonian <- new_calendar(
  name = "babylonian",
  "Bab",
  c("year", "month", "leap_month", "day"),
  validate = validate_babylonian,
  format_babylonian,
  babylonian_from_fixed,
  fixed_from_babylonian
)

#' Babylonian calendar dates
#'
#' The classical Babylonian calendar was a lunisolar calendar with a fixed 19-year Metonic cycle.
#'
#' @param year Numeric vector of years
#' @param month Numeric vector of months
#' @param leap_month Logical vector of leap months
#' @param day Numeric vector of days
#' @return A babylonian vector object
#' @seealso [cal_babylonian]
#' @examples
#' tibble::tibble(
#'   gregorian = gregorian_date(2335, 1, 1:2),
#'   babylonian = as_babylonian(gregorian)
#' )
#' babylonian_date(2335, 6, FALSE, 1:2)
#' @export
babylonian_date <- function(
  year = integer(),
  month = integer(),
  leap_month = logical(),
  day = integer()
) {
  new_date(
    year = year,
    month = month,
    leap_month = leap_month,
    day = day,
    calendar = cal_babylonian
  )
}

#' @rdname babylonian_date
#' @param date Vector of dates on some calendar.
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
  out <- (moon - sun) / 24
  out[is.na(moon)] <- hr(24) # Arbitrary
  out
}

babylonian_leap_year_p <- function(b_year) {
  # True if b_year is a leap year on Babylonian calendar
  ((7 * b_year + 13) %% 19) < 7
}

babylonian_criterion <- function(date) {
  # Moonlag criterion for visibility of crescent moon on
  # eve of date in Babylon
  set <- sunset(date - 1, BABYLON)
  set <- date - 1 + as.numeric(set) / 24
  tee <- universal_from_standard(set, BABYLON)
  phase <- lunar_phase(tee)

  (NEW < phase & phase < FIRST_QUARTER) &
    (nth_new_moon(new_moon_before(tee)) <= (tee - hr(24))) &
    (moonlag(date - 1, BABYLON) > mn(48))
}

babylonian_new_month_on_or_before <- function(date) {
  # Fixed date of start of Babylonian month on or before
  # Babylonian date. Using lag of moonset criterion
  moon <- fixed_from_moment(lunar_phase_at_or_before(
    rep(NEW, length(date)),
    date
  ))
  age <- date - moon
  tau <- moon - 30 * as.numeric(age <= 3 & !babylonian_criterion(date))
  next_value(tau, function(x) {
    babylonian_criterion(x)
  })
}
