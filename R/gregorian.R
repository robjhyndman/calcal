#==============================================================================
# Gregorian Calendar
#==============================================================================

#' Gregorian calendar dates
#'
#' Create a Gregorian date object.
#'
#' @param year A numeric vector of years
#' @param month A numeric vector of months
#' @param day A numeric vector of days
#' @return A gregorian vector object
#' @examples
#' gregorian(2025, 4, 19:30)
#' @export
gregorian <- function(
    year = integer(),
    month = integer(),
    day = integer()) {
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
  prior_days <- date - gregorian_new_year(year)
  # Correction to simulate a 30-day Feb
  correction <- (date >= as_rd(gregorian(year, MARCH, 1))) *
    (2 - gregorian_leap_year(year))
  # Assuming a 30-day Feb
  month <- (12 * (prior_days + correction) + 373) %/% 367
  # Calculate the day by subtraction
  day <- 1 + date - as_rd(gregorian(year, month, 1))
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

last_day_of_gregorian_month <- function(g_year, g_month) {
  y <- g_year + g_month == 12
  m <- amod(g_month + 1, 12)
  gregorian(g_year, g_month, 1) - gregorian(y, m, 1)
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


gregorian_new_year <- function(g_year) {
  as_rd(gregorian(g_year, JANUARY, 1))
}

gregorian_year_end <- function(g_year) {
  as_rd(gregorian(g_year, DECEMBER, 31))
}

gregorian_year_range <- function(g_year) {
  c(gregorian_new_year(g_year), gregorian_new_year(g_year + 1))
}

#' @export
as.character.gregorian <- function(x, ...) {
  format(x)
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
#' \code{days_remaining()} returns the number of days remaining in the year. Other functions should be
#' self-explanatory.
#'
#' @param date A vector of Gregorian dates
#' @param numeric Logical. Return a numeric vector if TRUE with 1 denoting the \code{first_day}
#' @param first_day Character denoting first day of the week. Ignored if \code{numeric} is \code{FALSE}.
#' Default is \code{"Monday"}
#' @param abbreviate Logical. Return abbreviated day names if \code{TRUE}. Ignored if \code{numeric} is \code{TRUE}.
#' @examples
#' april2025 <- gregorian(2025, 4, 1:30)
#' day_of_week(april2025)
#' day_of_month(april2025)
#' day_of_year(april2025)
#' days_remaining(april2025)
#' week_of_month(april2025)
#' week_of_year(april2025)
#' month_of_year(april2025)
#' @rdname gregorian-parts
#' @export
day_of_week <- function(date, numeric = FALSE, first_day = "Monday", abbreviate = FALSE) {
  dow <- day_of_week_from_fixed(as_rd(date)) + 1
  if(numeric) {
    first_day <- pmatch(first_day, c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
    if(is.na(first_day)) {
      stop("I can't determine the first day of the week")
    }
    return((dow - first_day) %% 7 + 1)
  } else {
    if(abbreviate) {
      return(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[dow])
    } else {
      return(c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")[dow])
    }
  }
}

#' @rdname gregorian-parts
#' @export
day_of_month <- function(date) {
  field(date, "day")
}


#' @rdname gregorian-parts
#' @export
# Day number in year of Gregorian date date
# Called day_number in CC book and code
day_of_year <- function(date) {
  as_rd(date) - as_rd(gregorian(field(date, "year") - 1, DECEMBER, 31))
}

#' @rdname gregorian-parts
#' @export
days_remaining <- function(date) {
  # Days remaining in year after Gregorian date
  as_rd(gregorian(field(date, "year"), DECEMBER, 31)) - as_rd(date)
}

#' @rdname gregorian-parts
#' @export
week_of_month <- function(date, first_day = "Monday") {
  dow <- day_of_week(date, numeric = TRUE, first_day = first_day)
  date <- date + (4 - dow)
  day1 <- gregorian(field(date, "year"), field(date, "month"), 1)
  (date - day1) %/% 7 + 1
}

#' @rdname gregorian-parts
#' @export
week_of_year <- function(date, first_day = "Monday") {
  dow <- day_of_week(date, numeric = TRUE, first_day = first_day)
  date <- date + (4 - dow)
  jan1 <- gregorian(field(date, "year"), JANUARY, 1)
  (date - jan1) %/% 7 + 1
}


#' @rdname gregorian-parts
#' @export
month_of_year <- function(date) {
  field(date, "month")
}
