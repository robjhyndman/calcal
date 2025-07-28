#==============================================================================
# Persian Calendar
#==============================================================================

PERSIAN_EPOCH <- 226896 # vec_data(julian_date(ce(622), MARCH, 19))
TEHRAN <- location(angle(35.68, 0, 0), angle(51.42, 0, 0), mt(1100), 3.5)

fixed_from_persian <- function(p_date) {
  miss <- is.na(p_date$year) | is.na(p_date$month) | is.na(p_date$day)
  new_year <- rep(NA_real_, length(p_date$year))
  new_year[!miss] <- persian_new_year_on_or_before(
    PERSIAN_EPOCH +
      180 +
      floor(MEAN_TROPICAL_YEAR * (p_date$year[!miss] - (p_date$year[!miss] > 0)))
  )
  new_year -
    1 + # Days in prior years
    (p_date$month - 1) * 30 +
    (p_date$month - 1) * (p_date$month <= 7) +
    6 * (p_date$month > 7) +
    p_date$day # Days so far this month
}

persian_from_fixed <- function(date) {
  date <- vec_data(date)
  miss <- is.na(date)
  new_year <- rep(NA_real_, length(date))
  new_year[!miss] <- persian_new_year_on_or_before(date[!miss])
  y <- round((new_year - PERSIAN_EPOCH) / MEAN_TROPICAL_YEAR) + 1
  year <- y - (y <= 0) # No year zero
  day_of_year <- 1 + date - vec_data(persian_date(year, 1, 1))
  month <- ceiling(day_of_year / 31)
  month[day_of_year > 186 & !miss] <- ceiling((day_of_year[day_of_year > 186 & !miss] - 6) / 30)
  day <- date - (vec_data(persian_date(year, month, 1)) - 1)

  list(year = year, month = month, day = trunc(day))
}

fixed_from_arithmetic_persian <- function(p_date) {
  y <- p_date$year - 473 - (p_date$year > 0) # Years since start of 2820-year cycle
  year <- (y %% 2820) + 474 # Equivalent year in the range 474..3263

  PERSIAN_EPOCH -
    1 + # Days before epoch
    1029983 * (y %/% 2820) + # Days in 2820-year cycles before Persian year 474
    365 * (year - 1) + # Nonleap days in prior years this 2820-year cycle
    ((31 * year - 5) %/% 128) + # Leap days in prior years this 2820-year cycle
    (p_date$month - 1) * 30 +
    (p_date$month - 1) * (p_date$month <= 7) +
    6 * (p_date$month > 7) +
    p_date$day # Days so far this month
}

arithmetic_persian_from_fixed <- function(date) {
  date <- vec_data(date)
  miss <- is.na(date)
  year <- arithmetic_persian_year_from_fixed(date)
  day_of_year <- 1 +
    date -
    vec_data(apersian_date(year, 1, 1))
  month <- ceiling(day_of_year / 31)
  month[day_of_year > 186 & !miss] <- ceiling((day_of_year[day_of_year > 186 & !miss] - 6) / 30)
  day <- date -
    (vec_data(apersian_date(year, month, 1)) - 1)
  list(year = year, month = month, day = trunc(day))
}

validate_persian <- function(date) {
  if (any(date$month < 1 | date$month > 12)) {
    stop("Month must be between 1 and 12")
  }
  if (any(date$day < 1 | date$day > 31)) {
    stop("Day must be between 1 and 31")
  }
  if (any(date$day == 31 & date$month > 7)) {
    stop("Only first 7 months can have 31 days")
  }
}

format_persian <- function(date) {
  format_date(
    date,
    month_name = c(
      "Farv",
      "Ordi",
      "Khor",
      "Tir",
      "Mord",
      "Shah",
      "Mehr",
      "Aban",
      "Azar",
      "Dey",
      "Bahm",
      "Esfa"
    )
  )
}

#' @rdname new_calendar
#' @format NULL
#' @export
cal_persian <- new_calendar(
  name = "persian",
  short_name = "Per",
  c("year", "month", "day"),
  validate_persian,
  format_persian,
  persian_from_fixed,
  fixed_from_persian
)

#' @rdname new_calendar
#' @format NULL
#' @export
cal_apersian <- new_calendar(
  name = "apersian",
  short_name = "APer",
  c("year", "month", "day"),
  validate_persian,
  format_persian,
  arithmetic_persian_from_fixed,
  fixed_from_arithmetic_persian
)

#' Persian dates
#'
#' The modern Persian calendar was adopted in 1925 in Iran and in 1957 in Afghanistan. An alternative version
#' of the calendar, using only arithmetic (rather than astronomical) calculations is available as the `apersian` calendar.
#'
#' @param year Numeric vector of years
#' @param month Numeric vector of months
#' @param day Numeric vector of days
#' @return A persian vector object
#' @examples
#' gregorian_date(2025,5,1:20) |>
#'   as_persian()
#'
#' @export
persian_date <- function(year = integer(), month = integer(), day = integer()) {
  new_date(year = year, month = month, day = day, calendar = cal_persian)
}

#' @rdname persian_date
#' @export
apersian_date <- function(
  year = integer(),
  month = integer(),
  day = integer()
) {
  new_date(year = year, month = month, day = day, calendar = cal_apersian)
}

#' @rdname persian_date
#' @param date Vector of dates on some calendar
#' @export
as_persian <- function(date) {
  as_date(date, calendar = cal_persian)
}

#' @rdname persian_date
#' @export
as_apersian <- function(date) {
  as_date(date, calendar = cal_apersian)
}

midday_in_tehran <- function(date) {
  midday(date, TEHRAN)
}

persian_new_year_on_or_before <- function(date) {
  approx <- estimate_prior_solar_longitude(SPRING, midday_in_tehran(date))
  next_value(floor(approx) - 1, function(day) {
    solar_longitude(midday_in_tehran(day)) <= SPRING + deg(2)
  })
}

arithmetic_persian_leap_year <- function(p_year) {
  y <- p_year - 473 - (p_year > 0) # Years since start of 2820-year cycles
  year <- (y %% 2820) + 474 # Equivalent year in the range 474..3263
  ((year + 38) * 31) %% 128 < 31
}


arithmetic_persian_year_from_fixed <- function(date) {
  # Prior days since start of 2820-year cycle beginning in Persian year 474
  d0 <- date - vec_data(apersian_date(475, 1, 1))
  n2820 <- d0 %/% 1029983 # Completed prior 2820-year cycles
  d1 <- d0 %% 1029983 # Prior days not in n2820

  # Years since start of last 2820-year cycle
  y2820 <- (128 * d1 + 46878) %/% 46751
  y2820[d1 == 1029982] <- 2820 # Last day of 2820-year cycle

  # Years since Persian epoch
  year <- 474 + 2820 * n2820 + y2820

  year - (year <= 0) # No year zero
}

nowruz <- function(g_year) {
  persian_year <- 1 + (g_year - gregorian_year_from_fixed(PERSIAN_EPOCH))
  y <- persian_year - (persian_year <= 0) # No Persian year 0

  vec_data(persian_date(y, 1, 1)) |> as_gregorian()
}


#' @export
day_of_week.persian <- function(date, ...) {
  dow <- day_of_week_from_fixed(date) + 1
  c(
    "Shanbeh",
    "Yekshanbeh",
    "Dushanbeh",
    "Seshanbeh",
    "Chaharshanbeh",
    "Panjshanbeh",
    "Jomeh"
  )[dow]
}

#' @export
day_of_week.apersian <- function(date, ...) {
  day_of_week.persian(date, ...)
}
