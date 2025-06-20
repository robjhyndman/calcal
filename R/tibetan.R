# Tibetan Calendar Functions

# Tibetan epoch constant
TIBETAN_ENOCH <- -46410 # vec_data(gregorian_date(-127, DECEMBER, 7))

fixed_from_tibetan <- function(t_date) {
  # Lunar month count
  months <- floor(
    804 /
      65 *
      (t_date$year - 1) +
      67 / 65 * t_date$month -
      t_date$leap_month +
      64 / 65
  )
  # Lunar day count
  days <- 30 * months + t_date$day
  # Mean civil days since epoch
  mean <- days * 11135 / 11312 - 30 - as.numeric(!t_date$leap_day) + 1071 / 1616
  solar_anomaly <- (days * 13 / 4824 + 2117 / 4824) %% 1
  lunar_anomaly <- (days * 3781 / 105840 + 2837 / 15120) %% 1
  sun <- -tibetan_sun_equation(12 * solar_anomaly)
  moon <- tibetan_moon_equation(28 * lunar_anomaly)

  floor(TIBETAN_ENOCH + mean + sun + moon)
}

tibetan_from_fixed <- function(date) {
  # Tibetan lunar date corresponding to fixed date
  cap_Y <- 365 + 4975 / 18382 # Average Tibetan year
  years <- ceiling((vec_data(date) - TIBETAN_ENOCH) / cap_Y)
  # Search for year
  year0 <- years
  while (any(date >= tibetan_date(year0, 1, FALSE, 1, FALSE))) {
    j <- which(date >= tibetan_date(year0, 1, FALSE, 1, FALSE))
    year0[j] <- year0[j] + 1
  }
  year0 <- year0 - 1
  # Search for month
  month0 <- rep(1, length(year0))
  while (any(date >= tibetan_date(year0, month0, FALSE, 1, FALSE))) {
    j <- which(date >= tibetan_date(year0, month0, FALSE, 1, FALSE))
    month0[j] <- month0[j] + 1
  }
  month0 <- month0 - 1
  # Search for day
  day0 <- date - tibetan_date(year0, month0, FALSE, 1, FALSE) - 2
  while (any(date >= tibetan_date(year0, month0, FALSE, day0, FALSE))) {
    j <- which(date >= tibetan_date(year0, month0, FALSE, day0, FALSE))
    day0[j] <- day0[j] + 1
  }
  day0 <- day0 - 1

  leap_month <- day0 > 30
  day <- amod(day0, 30)
  case1 <- day > day0
  case2 <- !case1 & leap_month
  month <- amod(month0 - case1 + case2, 12)
  case1 <- day > day0 & month0 == 1
  case2 <- leap_month & month0 == 12
  year <- year0 - case1 + case2
  leap_day <- (date == tibetan_date(year, month, leap_month, day, TRUE))

  list(
    year = year,
    month = month,
    leap_month = leap_month,
    day = day,
    leap_day = leap_day
  )
}

validate_tibetan <- function(date) {
  # Validation causes errors because some calculations use invalid dates
  #  if (any(date$month < 0 | date$month > 13)) {
  #    stop("Month must be between 1 and 12")
  #  }
  #  if (any(date$day < 1 | date$day > 30)) {
  #    stop("Day must be between 1 and 30")
  #  }
}

cal_tibetan <- cal_calendar(
  name = "tibetan",
  "Tib",
  c("year", "month", "leap_month", "day", "leap_day"),
  validate = validate_tibetan,
  format_date,
  tibetan_from_fixed,
  fixed_from_tibetan
)

#' Tibetan dates
#'
#' @param year A numeric vector of years
#' @param month A numeric vector of months
#' @param leap_month A logical vector of leap months
#' @param day A numeric vector of days
#' @param leap_day A logical vector of leap days
#' @seealso [tibetan_new_year]
#' @examples
#' gregorian_date(2025,6,1:10) |> as_tibetan()
#' @return A `tibetan_date` object
#'
#' @export
tibetan_date <- function(year, month, leap_month, day, leap_day) {
  new_date(
    year = year,
    month = month,
    leap_month = leap_month,
    day = day,
    leap_day = leap_day,
    calendar = cal_tibetan
  )
}

#' @rdname tibetan_date
#' @param date A vector of dates on some calendar
#' @export
as_tibetan <- function(date) {
  as_date(date, calendar = cal_tibetan)
}

tibetan_sun_equation <- function(alpha) {
  # Create lookup table for the base interval [0, 6]
  get_base_value <- function(x) {
    integer_vals <- mins(c(0, 6, 10, 11))
    ifelse(
      x == floor(x) & x <= 3,
      integer_vals[x + 1],
      {
        # For x > 3, use symmetry: f(x) = f(6-x)
        x_sym <- ifelse(x > 3, 6 - x, x)
        # Linear interpolation
        floor_x <- floor(x_sym)
        ceil_x <- ceiling(x_sym)
        frac <- x_sym - floor_x
        floor_val <- ifelse(
          floor_x <= 3,
          integer_vals[floor_x + 1],
          integer_vals[4]
        )
        ceil_val <- ifelse(
          ceil_x <= 3,
          integer_vals[ceil_x + 1],
          integer_vals[4]
        )
        (1 - frac) * floor_val + frac * ceil_val
      }
    )
  }

  # Normalize to [0, 12) and apply periodicity
  alpha_norm <- alpha %% 12
  sign_factor <- ifelse(alpha_norm > 6, -1, 1)
  alpha_base <- ifelse(alpha_norm > 6, 12 - alpha_norm, alpha_norm)
  sign_factor * get_base_value(alpha_base)
}

tibetan_moon_equation <- function(alpha) {
  # Create lookup table for integer values [0, 7]
  get_base_value <- function(x) {
    # Lookup table for exact integer cases
    integer_vals <- mins(c(0, 5, 10, 15, 19, 22, 24, 25))
    ifelse(
      x == floor(x) & x <= 7,
      integer_vals[x + 1], 
      {
        # For x > 7, use symmetry: f(x) = f(14-x)
        x_sym <- ifelse(x > 7, 14 - x, x)
        # Linear interpolation
        floor_x <- floor(x_sym)
        ceil_x <- ceiling(x_sym)
        frac <- x_sym - floor_x
        # Get values at floor and ceiling points
        floor_val <- ifelse(
          floor_x <= 7,
          integer_vals[floor_x + 1],
          integer_vals[8]
        )
        ceil_val <- ifelse(
          ceil_x <= 7,
          integer_vals[ceil_x + 1],
          integer_vals[8]
        )
        (1 - frac) * floor_val + frac * ceil_val
      }
    )
  }

  # Normalize to [0, 28) and apply periodicity (period = 28)
  alpha_norm <- alpha %% 28
  sign_factor <- ifelse(alpha_norm > 14, -1, 1)
  alpha_base <- ifelse(alpha_norm > 14, 28 - alpha_norm, alpha_norm)
  sign_factor * get_base_value(alpha_base)
}

tibetan_leap_month_p <- function(t_year, t_month) {
  # True if t_month is leap in Tibetan year t_year
  t_month ==
    granularity(
      tibetan_date(t_year, t_month, TRUE, 2, FALSE),
      "month"
    )
}

tibetan_leap_day_p <- function(t_year, t_month, t_day) {
  # True if t_day is leap in Tibetan month t_month and year t_year
  (t_day ==
    tibetan_date(t_year, t_month, FALSE, t_day, TRUE) |>
      granularity("day")) |
    # Check also in leap month if there is one
    (t_day ==
      tibetan_date(
        t_year,
        t_month,
        tibetan_leap_month_p(t_year, t_month),
        t_day,
        TRUE
      ) |>
        granularity("day"))
}

#' Tibetan holidays
#' 
#' The Tibetan New Year occurs on the first day of the Tibetan calendar. These
#' functions calculate the date given either a Gregorian year or a Tibetan year.
#' Both return a Gregorian date.
#' 
#' @param year A vector of Gregorian years
#' @param t_year A vector of Tibetan years
#' @examples
#' tibetan_new_year(2025:2028)
#' losar(2152:2154)
#' @seealso [tibetan_date]
#' @export
tibetan_new_year <- function(year) {
  dec31 <- gregorian_year_end(year) |> vec_data()
  t_year <- tibetan_from_fixed(dec31)$year
  losars <- losar(seq(min(t_year)-1 , max(t_year)))
  yr <- granularity(losars, "year")
  losars[yr %in% year]
}

#' @rdname tibetan_new_year
#' @export
losar <- function(t_year) {
  t_leap <- tibetan_leap_month_p(t_year, 1)
  as_gregorian(tibetan_date(t_year, 1, t_leap, 1, FALSE))
}

