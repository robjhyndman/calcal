# ==============================================================================
# Islamic Calendar
# ==============================================================================

ISLAMIC_EPOCH <- 227015 #vec_data(julian_date(ce(622), JULY, 16))

check_islamic <- function(args) {
  year <- args$year
  month <- args$month
  day <- args$day
  if (any(month < 1 | month > 12)) {
    stop("month must be between 1 and 12")
  }
  if (any(day < 1 | day > 30)) {
    stop("day must be between 1 and 30")
  }
}

# Register format method for islamic_date
format_islamic <- function(x, ...) {
  format_date(
    x,
    month_name = c(
      "Muh",
      "Saf",
      "Rab1",
      "Rab2",
      "Jum1",
      "Jum2",
      "Raj",
      "Sha",
      "Ram",
      "Shaw",
      "Dhu'l_Q",
      "Dhu'l_H"
    )
  )
}

fixed_from_islamic <- function(date, ...) {
  ISLAMIC_EPOCH -
    1 +
    354 * (date$year - 1) +
    (3 + 11 * date$year) %/% 30 +
    29 * (date$month - 1) +
    date$month %/% 2 +
    date$day
}

islamic_from_fixed <- function(date, ...) {
  year <- (30 * (vec_data(date) - ISLAMIC_EPOCH) + 10646) %/% 10631
  prior_days <- date - islamic_date(year, 1, 1)
  month <- (11 * prior_days + 330) %/% 325
  day <- date - islamic_date(year, month, 1) + 1
  list(year = year, month = month, day = day)
}

#' @rdname cal_calendar
#' @format NULL
#' @export
cal_islamic <- cal_calendar(
  name = "Islamic",
  short_name = "Hij",
  granularities = c("year", "month", "day"),
  check_granularities = check_islamic,
  format = format_islamic,
  from_rd = islamic_from_fixed,
  to_rd = fixed_from_islamic
)

#' Islamic dates
#'
#' @examples
#' islamic_date(2025, 5, 1:30)
#' as_islamic("2016-01-01")
#' as_islamic(Sys.Date())
#' tibble::tibble(
#'   x = seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"),
#'   y = as_islamic(x)
#' )
#' @examples
#' islamic_date(2025, 4, 19:30)
#' @seealso [cal_islamic]
#' @rdname islamic
#' @param year A numeric vector of years
#' @param month A numeric vector of months
#' @param day A numeric vector of days
#' @return An islamic vector object
#' @export
islamic_date <- function(year = integer(), month = integer(), day = integer()) {
  new_date(year = year, month = month, day = day, calendar = cal_islamic)
}

#' @rdname islamic
#' @param date Vector of dates on some calendar
#' @export
as_islamic <- function(date) {
  as_date(date, calendar = cal_islamic)
}

islamic_leap_year <- function(i_year) {
  (i_year * 11 + 14) %% 30 < 11
}

islamic_in_gregorian <- function(i_month, i_day, g_year) {
  jan1 <- gregorian_new_year(g_year)
  y <- standard_year(as_islamic(jan1))
  date0 <- islamic_date(y, i_month, i_day)
  date1 <- islamic_date(y + 1, i_month, i_day)
  date2 <- islamic_date(y + 2, i_month, i_day)
  dates3_in_gregorian(g_year, date0, date1, date2)
}

#' Islamic holidays
#'
#' Functions to return Gregorian dates for various Islamic holidays. Specific
#' dates can vary slightly based on moon sightings in different regions.
#'
#' @param year A numeric vector of Gregorian years
#' @return A list of dates on the Gregorian calendar
#'
#' @examples
#' tibble::tibble(
#'   year = 2025:2029,
#'   `New year` = islamic_new_year(year),
#'   Mawlid = mawlid(year),
#'   Ramadan = ramadan(year),
#'   `Eid al-Fitr` = eid_al_fitr(year),
#'   `Eid al-Adha` = eid_al_adha(year)
#' )
#' ramadan(2030)
#' @rdname islamic_holidays
#' @export
# Islamic New Year
islamic_new_year <- function(year) {
  as_gregorian(islamic_in_gregorian(1, 1, year))
}

#' @rdname islamic_holidays
#' @export
mawlid <- function(year) {
  as_gregorian(islamic_in_gregorian(2, 12, year))
}

#' @rdname islamic_holidays
#' @export
# Ramadan, the month of fasting
ramadan <- function(year) {
  as_gregorian(islamic_in_gregorian(9, 1, year))
}

#' @rdname islamic_holidays
#' @export
# Eid al-Fitr
eid_al_fitr <- function(year) {
  as_gregorian(islamic_in_gregorian(10, 1, year))
}

#' @rdname islamic_holidays
#' @export
# Eid al-Adha
eid_al_adha <- function(year) {
  as_gregorian(islamic_in_gregorian(12, 10, year))
}
