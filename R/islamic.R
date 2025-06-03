#==============================================================================
# Islamic Calendar
#==============================================================================

#' Islamic calendar dates
#'
#' Create an Islamic date object.
#'
#' @param year A numeric vector of years
#' @param month A numeric vector of months
#' @param day A numeric vector of days
#' @return An islamic vector object
#' @examples
#' islamic_date(2025, 4, 19:30)
#' @rdname islamic_date
#' @export
islamic_date <- function(
  year = integer(),
  month = integer(),
  day = integer()
) {
  lst <- vec_cast_common(year = year, month = month, day = day, .to = integer())
  lst <- vec_recycle_common(
    year = lst$year,
    month = lst$month,
    day = lst$day,
    .size = max(unlist(lapply(lst, length)))
  )
  check_islamic(lst)
  new_rcrd(lst, class = c("islamic", "calcalcal"))
}

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
#' @export
format.islamic <- function(x, ...) {
  paste(
    sprintf("%.2d", year(x)),
    c(
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
    )[field(x, "month")],
    sprintf("%.2d", field(x, "day")),
    sep = "-"
  )
}

#' @export
vec_ptype_abbr.islamic <- function(x, ...) {
  "Hij"
}

islamic_leap_year <- function(i_year) {
  (i_year * 11 + 14) %% 30 < 11
}

#' @export
as_rd.islamic <- function(date, ...) {
  month <- standard_month(date)
  day <- standard_day(date)
  year <- standard_year(date)

  rd_fixed(
    ISLAMIC_EPOCH -
      1 +
      354 * (year - 1) +
      (3 + 11 * year) %/% 30 +
      29 * (month - 1) +
      month %/% 2 +
      day
  )
}

#' Convert to an Islamic date
#'
#' @param date Vector of dates on some calendar
#' @param ... Additional arguments not currently used
#' @rdname islamic_date
#' @examples
#' as_islamic("2016-01-01")
#' as_islamic(Sys.Date())
#' tibble::tibble(
#'   x = gregorian_date(2025, 5, 1:31),
#'   y = as_islamic(x)
#' )
#' @export
as_islamic <- function(date, ...) {
  UseMethod("as_islamic")
}

#' @export
as_islamic.rd_fixed <- function(date, ...) {
  year <- (30 * (vec_data(date) - ISLAMIC_EPOCH) + 10646) %/% 10631
  prior_days <- date - as_rd(islamic_date(year, 1, 1))
  month <- (11 * prior_days + 330) %/% 325
  day <- date - as_rd(islamic_date(year, month, 1)) + 1
  islamic_date(year, month, day)
}

#' @export
as_islamic.default <- function(date, ...) {
  as_islamic(as_rd(date))
}

islamic_in_gregorian <- function(i_month, i_day, g_year) {
  jan1 <- gregorian_new_year(g_year)
  y <- standard_year(as_islamic(jan1))
  date0 <- vec_data(as_rd(islamic_date(y, i_month, i_day)))
  date1 <- vec_data(as_rd(islamic_date(y + 1, i_month, i_day)))
  date2 <- vec_data(as_rd(islamic_date(y + 2, i_month, i_day)))
  out <- list_range(c(date0, date1, date2), gregorian_year_range(g_year))
  out <- mapply(
    function(d0, d1, d2, year) {
      list_range(c(d0, d1, d2), gregorian_year_range(year))
    },
    date0,
    date1,
    date2,
    g_year
  )
  l <- lapply(out, length)
  out <- out[l > 0]
  if (length(out) > 0) {
    as_gregorian(out)
  } else {
    gregorian_date()
  }
}

#' Islamic holidays
#'
#' @param year A numeric vector of Gregorian years
#' @return A list of dates on the Gregorian calendar

mawlid <- function(year) {
  islamic_in_gregorian(3, 12, year)
}
