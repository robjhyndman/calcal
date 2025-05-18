#==============================================================================
# ISO Calendar
#==============================================================================

#' ISO calendar dates
#'
#' Create an ISO 8601 date object. Weeks are defined as starting on Mondays. Week 1
#' is the first week with at least 4 days in the year. Equivalently, it is the week
#' containing 4 January. There is no week 0; instead week 1 of a year may begin in
#' the previous calendar year.
#'
#' More flexible week numbering is possible using Gregorian dates with \code{\link{week_of_year}()}.
#'
#' @param year A numeric vector of years
#' @param week A numeric vector of weeks
#' @param day A numeric vector of days
#' @return An iso vector object
#' @seealso \code{\link{week_of_year}()}
#' @examples
#' iso_date(2025, 23, 2:4)
#' as_gregorian(iso_date(2025, 23, 2:4))
#' as_iso(gregorian(2025, 1, 1:31))
#' @export
iso_date <- function(
  year = integer(),
  week = integer(),
  day = integer()) {
lst <- vec_cast_common(year = year, week = week, day = day, .to = integer())
lst <- vec_recycle_common(year = lst$year, week = lst$week, day = lst$day)
check_iso(lst)
new_rcrd(lst, class = "iso")
}

check_iso <- function(args) {
  year <- args$year
  week <- args$week
  day <- args$day
  if (any(week < 1 | week > 53, na.rm = TRUE)) {
    stop("week must be between 1 and 53")
  }
  if (any(day < 1 | day > 7, na.rm = TRUE)) {
    stop("day must be between 1 and 7")
  }
}

iso_week <- function(date) {
  field(date, "week")
}

iso_day <- function(date) {
  field(date, "day")
}

iso_year <- function(date) {
  field(date, "year")
}

# Register format method for julian_date
#' @export
format.iso <- function(x, ...) {
  format_date(x)
}

#' Convert to an ISO date
#'
#' @param date Vector of dates on some calendar
#' @param ... Additional arguments not currently used
#' @rdname iso_date
#' @examples
#' as_iso("2016-01-01")
#' as_iso(Sys.Date())
#' tibble::tibble(
#'   date = seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"),
#'   iso = as_iso(date)
#' )
#' @export
as_iso <- function(date, ...) {
  UseMethod("as_iso")
}

#' @export
as_rd.iso <- function(date, ...) {
  week <- iso_week(date)
  day <- iso_day(date)
  year <- iso_year(date)

  nth_kday(week, SUNDAY, gregorian(year - 1, DECEMBER, 28)) + day
}

#' @export
as_iso.rd_fixed <- function(date, ...) {
  approx <- gregorian_year_from_fixed(date - 3)
  year <- approx + (date >= as_rd(iso_date(approx + 1, 1, 1)))
  week <- 1 + (date - as_rd(iso_date(year, 1, 1))) %/% 7
  day <- amod(vec_data(date), 7L)
  iso_date(year, week, day)
}

#' @export
as_iso.default <- function(date, ...) {
  as_iso(as_rd(date))
}

iso_long_year <- function(i_year) {
  jan1 <- day_of_week_from_fixed(gregorian_new_year(i_year))
  dec31 <- day_of_week_from_fixed(gregorian_year_end(i_year))
  jan1 == THURSDAY | dec31 == THURSDAY
}

#' @export
day_of_year.iso <- function(date, ...) {
  date - iso_date(field(date, "year"), 1, 1) + 1
}


# Arithmetic

#' @export
#' @method vec_arith iso
vec_arith.iso <- function(op, x, y, ...) {
  UseMethod("vec_arith.iso", y)
}
#' @export
#' @method vec_arith.iso iso
vec_arith.iso.iso <- function(op, x, y, ...) {
  vec_arith(op, as_rd(x), as_rd(y))
}
#' @export
#' @method vec_arith.numeric iso
vec_arith.numeric.iso <- function(op, x, y, ...) {
  as_iso(vec_arith(op, x, as_rd(y)))
}
#' @export
#' @method vec_arith.iso numeric
vec_arith.iso.numeric <- function(op, x, y, ...) {
  as_iso(vec_arith(op, as_rd(x), y))
}
