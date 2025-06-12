# ==============================================================================
# Gregorian Calendar
# ==============================================================================

# Month constants for Julian/Gregorian calendar
JANUARY <- 1L
FEBRUARY <- JANUARY + 1L
MARCH <- JANUARY + 2L
APRIL <- JANUARY + 3L
MAY <- JANUARY + 4L
JUNE <- JANUARY + 5L
JULY <- JANUARY + 6L
AUGUST <- JANUARY + 7L
SEPTEMBER <- JANUARY + 8L
OCTOBER <- JANUARY + 9L
NOVEMBER <- JANUARY + 10L
DECEMBER <- JANUARY + 11L

# Day of week constants
SUNDAY <- 0L
MONDAY <- SUNDAY + 1L
TUESDAY <- SUNDAY + 2L
WEDNESDAY <- SUNDAY + 3L
THURSDAY <- SUNDAY + 4L
FRIDAY <- SUNDAY + 5L
SATURDAY <- SUNDAY + 6L

GREGORIAN_EPOCH <- 1L # Fixed date of start of the (proleptic) Gregorian calendar

validate_gregorian <- function(date) {
  if (any(date$month < 1 | date$month > 12, na.rm = TRUE)) {
    stop("month must be between 1 and 12")
  } else if (
    any(
      date$day > 30 & date$month %in% c(APRIL, JUNE, SEPTEMBER, NOVEMBER),
      na.rm = TRUE
    )
  ) {
    stop("day must be between 1 and 30")
  } else if (any(date$day > 29 & date$month == FEBRUARY, na.rm = TRUE)) {
    stop("days in February must be between 1 and 29")
  } else if (
    any(
      date$day > 28 & date$month == FEBRUARY & !gregorian_leap_year(date$year),
      na.rm = TRUE
    )
  ) {
    stop("days in February must be between 1 and 28 when not a leap year")
  } else if (any(date$day < 1 | date$day > 31, na.rm = TRUE)) {
    stop("day must be between 1 and 31")
  }
}

# Register format method for gregorian date
format_gregorian <- function(x, ...) {
  format_date(x, month_name = month.abb)
}

# Convert gregorian to RD
fixed_from_gregorian <- function(date, ...) {
  result <- GREGORIAN_EPOCH -
    1 + # Days before start of calendar
    365 * (date$year - 1) + # Ordinary days since epoch
    (date$year - 1) %/% 4 - # Julian leap days since epoch...
    (date$year - 1) %/% 100 + # ...minus century years since epoch...
    (date$year - 1) %/% 400 + # ...plus years since epoch divisible by 400
    (367 * date$month - 362) %/% 12 # Days in prior months this year...
  # Adjust for leap years
  adjustment <- (date$month > 2) * (-2 + gregorian_leap_year(date$year))
  result + adjustment + date$day
}

gregorian_from_fixed <- function(date, ...) {
  if (length(date) == 0L) {
    return(gregorian_date())
  }
  # Gregorian (year month day) corresponding to fixed date
  year <- gregorian_year_from_fixed(date)
  prior_days <- date - gregorian_new_year(year)
  # Correction to simulate a 30-day Feb
  correction <- (date >= gregorian_date(year, MARCH, 1)) *
    (2 - gregorian_leap_year(year))
  # Assuming a 30-day Feb
  month <- (12 * (prior_days + correction) + 373) %/% 367
  # Calculate the day by subtraction
  day <- 1 + date - gregorian_date(year, month, 1)
  list(year = year, month = month, day = day)
}

#' @rdname cal_calendar
#' @format NULL
#' @export
cal_gregorian <- cal_calendar(
  name = "gregorian",
  short_name = "Gre",
  granularities = c("year", "month", "day"),
  validate_granularities = validate_gregorian,
  format = format_gregorian,
  from_rd = gregorian_from_fixed,
  to_rd = fixed_from_gregorian
)

#' Gregorian calendar dates
#'
#' @param year A numeric vector of years
#' @param month A numeric vector of months
#' @param day A numeric vector of days
#' @seealso [cal_gregorian]
#' @return A gregorian vector object
#' @examples
#' new_date(year = 2025, month = 3, day = 2:4, calendar = cal_gregorian)
#' gregorian_date(2025, 4, 19:30)
#' as_date(Sys.Date(), calendar = cal_gregorian)
#' as_gregorian(Sys.Date())
#' as_gregorian("2016-01-01")
#' tibble::tibble(
#'   x = seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"),
#'   y = as_gregorian(x),
#'   z = as_date(x, calendar = cal_gregorian)
#' )
#' @rdname gregorian
#' @export
gregorian_date <- function(
  year = integer(),
  month = integer(),
  day = integer()
) {
  new_date(year = year, month = month, day = day, calendar = cal_gregorian)
}

#' @param date Vector of dates on some calendar
#' @rdname gregorian
#' @export
as_gregorian <- function(date) {
  as_date(date, calendar = cal_gregorian)
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

last_day_of_gregorian_month <- function(g_year, g_month) {
  y <- g_year + (g_month == 12)
  m <- amod(g_month + 1, 12)
  gregorian_date(y, m, 1) - gregorian_date(g_year, g_month, 1)
}

gregorian_new_year <- function(g_year) {
  gregorian_date(g_year, JANUARY, 1)
}

gregorian_year_end <- function(g_year) {
  gregorian_date(g_year, DECEMBER, 31)
}

gregorian_year_range <- function(g_year) {
  # Range of days in a vector of Gregorian years (from first to last)
  c(gregorian_new_year(min(g_year)), gregorian_new_year(max(g_year) + 1) - 1)
}

#' Extract parts of a Gregorian date
#'
#' Extract days, weeks, or months from a vector of Gregorian dates.
#'
#' @details
#' \code{week_of_year()} returns the ISO 8601 week number with \code{first_day} as Monday.
#' Under this standard, week 1 of a year is defined as the first week with at least 4 days in the year;
#' equivalently, it is the week  containing 4 January. There is no week 0; instead week 1 of a year may
#' begin in the previous calendar year.
#'
#' \code{week_of_month()} is defined analogously where week 1 of a month is the first week with at least
#' 4 days in the month; equivalently, it is the week containing the 4th day of the month. There is no week 0;
#' instead week 1 of a month may begin in the previous calendar month.
#'
#' \code{days_remaining()} returns the number of days remaining in the year.
#'
#' Other functions should be self-explanatory.
#'
#' @param date A vector of Gregorian dates
#' @param numeric Logical. Return a numeric vector if TRUE with 1 denoting the \code{first_day}
#' @param first_day Character denoting first day of the week. Ignored if \code{numeric} is \code{FALSE}.
#' Default is \code{"Monday"}
#' @param abbreviate Logical. Return abbreviated day names if \code{TRUE}. Ignored if \code{numeric} is \code{TRUE}.
#' @examples
#' april2025 <- gregorian_date(2025, 4, 1:30)
#' day_of_week(april2025)
#' day_of_month(april2025)
#' day_of_year(april2025)
#' days_remaining(april2025)
#' week_of_month(april2025)
#' week_of_year(april2025)
#' month_of_year(april2025)
#' @rdname gregorian-parts
#' @export
day_of_week <- function(
  date,
  numeric = FALSE,
  first_day = "Monday",
  abbreviate = FALSE
) {
  dow <- day_of_week_from_fixed(date) + 1
  if (numeric) {
    first_day <- pmatch(
      first_day,
      c(
        "Sunday",
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday"
      )
    )
    if (is.na(first_day)) {
      stop("I can't determine the first day of the week")
    }
    return((dow - first_day) %% 7 + 1)
  } else {
    if (abbreviate) {
      return(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[dow])
    } else {
      return(c(
        "Sunday",
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday"
      )[dow])
    }
  }
}

#' @rdname gregorian-parts
#' @export
day_of_month <- function(date) {
  if (!("month" %in% granularity_names(date))) {
    stop("Date must contain months")
  }
  granularity(date, "day")
}

#' @rdname gregorian-parts
#' @export
# Day number in year of Gregorian date date
# Called day_number in CC book and code
# Rewritten to work for any calendar with year granularity
day_of_year <- function(date) {
  gran <- granularity_names(date)
  if (!("year" %in% gran)) {
    stop("Date must contain years")
  }
  date0 <- base_granularities(date)
  for (f in gran[gran != "year"]) {
    date0[[f]] <- rep(1, length(date))
  }
  date0 <- attributes(date)$calendar$to_rd(date0)
  as.numeric(date - date0 + 1)
}

#' @rdname gregorian-parts
#' @export
days_remaining <- function(date) {
  gran <- granularity_names(date)
  if (!("year" %in% gran)) {
    stop("Date must contain years")
  }
  date0 <- base_granularities(date)
  date0[["year"]] <- date0[["year"]] + 1
  for (f in gran[gran != "year"]) {
    date0[[f]] <- rep(1, length(date))
  }
  date0 <- attributes(date)$calendar$to_rd(date0)
  as.numeric(date0 - date - 1)
}

#' @rdname gregorian-parts
#' @export
week_of_month <- function(date, first_day = "Monday") {
  dow <- day_of_week(date, numeric = TRUE, first_day = first_day)
  date <- date + (4 - dow)
  day1 <- gregorian_date(
    granularity(date, "year"),
    granularity(date, "month"),
    1
  )
  (date - day1) %/% 7 + 1
}

#' @rdname gregorian-parts
#' @export
week_of_year <- function(date, first_day = "Monday") {
  dow <- day_of_week(date, numeric = TRUE, first_day = first_day)
  date <- date + (4 - dow)
  jan1 <- gregorian_date(granularity(date, "year"), JANUARY, 1)
  (as_gregorian(date) - jan1) %/% 7 + 1
}

#' @rdname gregorian-parts
#' @export
month_of_year <- function(date) {
  granularity(date, "month")
}

#' @rdname gregorian-parts
#' @export
year <- function(date) {
  granularity(date, "year")
}
