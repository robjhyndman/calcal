#==============================================================================
# Mayan Calendars
#==============================================================================

MAYAN_EPOCH <- -1137142 # fixed_from_jd(584283) # August 11, -3113

fixed_from_mayan_long_count <- function(count) {
  count <- as.data.frame(count) |> as.matrix()
  MAYAN_EPOCH + from_radix(count, c(20, 20, 18, 20))
}

mayan_long_count_from_fixed <- function(date) {
  date <- vec_data(date) - MAYAN_EPOCH
  out <- to_radix(date, c(20, 20, 18, 20))
  list(
    baktun = out[, 1],
    katun = out[, 2],
    tun = out[, 3],
    uinal = out[, 4],
    kin = out[, 5]
  )
}

validate_mayan_long_count <- function(date) {
  if (any(date$kin < 0 | date$kin > 19)) {
    stop("kin must be between 0 and 19")
  }
  if (any(date$uinal < 0 | date$uinal > 19)) {
    stop("uinal must be between 0 and 19")
  }
  if (any(date$tun < 0 | date$tun > 17)) {
    stop("tun must be between 0 and 17")
  }
  if (any(date$katun < 0 | date$katun > 19)) {
    stop("katun must be between 0 and 19")
  }
}

#' @rdname cal_calendar
#' @format NULL
#' @export
cal_mayan <- cal_calendar(
  "mayan",
  "May",
  c("baktun", "katun", "tun", "uinal", "kin"),
  validate_mayan_long_count,
  format_date,
  mayan_long_count_from_fixed,
  fixed_from_mayan_long_count
)

#' Mayan dates
#'
#' There are three Mayan calendars: the famous "long count" calendar, the "Haab" calendar, and the "Tzolkin" calendar.
#' Of these, only the long count calendar can be converted to and from other calendars, so it is the only one that
#' has been implemented here.
#'
#' @param baktun Numeric vector
#' @param katun Numeric vector
#' @param tun Numeric vector
#' @param uinal Numeric vector
#' @param kin Numeric vector
#' @seealso [cal_mayan]
#' @examples
#' gregorian_date(2012, 12, 10:30) |>
#'   as_mayan()
#' @export
mayan_date <- function(baktun, katun, tun, uinal, kin) {
  new_date(
    baktun = baktun,
    katun = katun,
    tun = tun,
    uinal = uinal,
    kin = kin,
    calendar = cal_mayan
  )
}

#' @rdname mayan_date
#' @param date Vector of dates on some calendar
#' @export
as_mayan <- function(date) {
  as_date(date, calendar = cal_mayan)
}
