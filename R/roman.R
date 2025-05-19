# ==============================================================================
# Roman Calendar
# ==============================================================================

#' Roman calendar dates
#'
#' Create a Roman date object.
#'
#' @param year A numeric vector of years
#' @param month A numeric vector of months
#' @param event A numeric vector of events: 1 = KALENDS, 2 = NONES, 3 = IDES
#' @param count A numeric vector of counts:
#' @param leap A logical vector indicating if year is a leap year
#' @return A roman vector object
#' @examples
#' roman_date(66, 4, 1, 1, FALSE)
#' as_roman(Sys.Date())
#'
#' @export
roman_date <- function(
  year = integer(),
  month = integer(),
  event = integer(),
  count = integer(),
  leap = logical()
) {
  lst <- vec_cast_common(
    year = year,
    month = month,
    event = event,
    count = count,
    .to = integer()
  )
  lst$leap <- vec_cast(leap, logical())

  lst <- vec_recycle_common(
    year = lst$year,
    month = lst$month,
    event = lst$event,
    count = lst$count,
    leap = lst$leap,
    .size = max(unlist(lapply(lst, length)))
  )
  check_roman(lst)
  new_rcrd(lst, class = "roman_date")
}

check_roman <- function(args) {
  year <- args$year
  month <- args$month
  event <- args$event
  count <- args$count
  leap <- args$leap
  if (any(month < 1 | month > 12, na.rm = TRUE)) {
    stop("`month` must be between 1 and 12")
  }
  if (any(event < 1 | event > 3, na.rm = TRUE)) {
    stop("`event` must be between 1 and 3")
  }
  #if (any(count < 1 | count > 19, na.rm = TRUE)) {
  #  stop("`count` must be between 1 and 19")
  #}
}

#' @export
format.roman_date <- function(x, ...) {
  format_date(x)
}

#' @export
vec_ptype_abbr.roman_date <- function(x, ...) {
  "Rom"
}

roman_year <- function(date) {
  field(date, "year")
}

roman_month <- function(date) {
  field(date, "month")
}

roman_event <- function(date) {
  field(date, "event")
}

roman_count <- function(date) {
  field(date, "count")
}

roman_leap <- function(date) {
  field(date, "leap")
}

ides_of_month <- function(month) {
  13 + 2 * (month %in% c(MARCH, MAY, JULY, OCTOBER))
}

nones_of_month <- function(month) {
  ides_of_month(month) - 8
}

#' @export
as_rd.roman_date <- function(date, ...) {
  leap <- roman_leap(date)
  count <- roman_count(date)
  event <- roman_event(date)
  month <- roman_month(date)
  year <- roman_year(date)

  kalends <- as_rd(julian_date(year, month, 1)) # KALENDS
  nones <- as_rd(julian_date(year, month, nones_of_month(month))) # NONES
  ides <- as_rd(julian_date(year, month, ides_of_month(month))) # IDES
  base_date <- kalends
  base_date[event == 2] <- nones[event == 2]
  base_date[event == 3] <- ides[event == 3]

  base_date -
    count +
    as.numeric(
      !(julian_leap_year(year) &
        month == MARCH &
        event == KALENDS &
        count >= 6 &
        count <= 16)
    ) +
    as.numeric(leap)
}

#' @rdname roman_date
#' @param date A date object
#' @param ... Additional arguments not currently used
#' @export
as_roman <- function(date, ...) {
  UseMethod("as_roman")
}

#' @export
as_roman.default <- function(date, ...) {
  as_roman(as_rd(date))
}

#' @export
as_roman.rd_fixed <- function(date, ...) {
  j_date <- as_julian(date)
  month <- standard_month(j_date)
  day <- standard_day(j_date)
  year <- standard_year(j_date)
  month_prime <- amod(month + 1, 12)
  year_prime <- year + (month_prime == 1 & year != -1)
  year_prime[month_prime == 1 & year == -1] <- 1
  kalends1 <- as_rd(roman_date(year_prime, month_prime, KALENDS, 1, FALSE))
  case1 <- day == 1
  case2 <- !case1 & day <= nones_of_month(month)
  case3 <- !case1 & !case2 & day <= ides_of_month(month)
  case4 <- !case1 & !case2 & !case3 & (month != 2 | !julian_leap_year(year))
  case5 <- !case1 & !case2 & !case3 & !case4 & day < 25
  case6 <- !case1 & !case2 & !case3 & !case4 & !case5 & day == 25
  output <- roman_date(year, 3, KALENDS, 31 - day, day == 25)
  if (any(case1)) {
    output[case1] <- roman_date(year[case1], month[case1], KALENDS, 1, FALSE)
  }
  if (any(case2)) {
    output[case2] <- roman_date(
      year[case2],
      month[case2],
      NONES,
      nones_of_month(month[case2]) - day[case2] + 1,
      FALSE
    )
  }
  if (any(case3)) {
    output[case3] <- roman_date(
      year[case3],
      month[case3],
      IDES,
      ides_of_month(month[case3]) - day[case3] + 1,
      FALSE
    )
  }
  if (any(case4)) {
    output[case4] <- roman_date(
      year_prime[case4],
      month_prime[case4],
      KALENDS,
      kalends1[case4] - date[case4] + 1,
      FALSE
    )
  }
  if (any(case5)) {
    output[case5] <- roman_date(year[case5], 3, KALENDS, 30 - day[case5], FALSE)
  }
  if (any(case6)) {
    output[case6] <- roman_date(
      year[case6],
      3,
      KALENDS,
      31 - day[case6],
      day[case6] == 25
    )
  }
  output
}

julian_year_from_auc <- function(year) {
  year + YEAR_ROME_FOUNDED - (1 <= year & year <= -YEAR_ROME_FOUNDED)
}

auc_year_from_julian <- function(year) {
  year - YEAR_ROME_FOUNDED + (YEAR_ROME_FOUNDED <= year & year <= -1)
}

olympiad <- function(cycle, year) {
  c(cycle, year)
}

olympiad_cycle <- function(o_date) {
  o_date[1]
}

olympiad_year <- function(o_date) {
  o_date[2]
}

olympiad_from_julian_year <- function(j_year) {
  years <- j_year - OLYMPIAD_START - (j_year >= 0)
  olympiad(1 + years %/% 4, 1 + years %% 4)
}

julian_year_from_olympiad <- function(o_date) {
  cycle <- olympiad_cycle(o_date)
  year <- olympiad_year(o_date)
  OLYMPIAD_START + 4 * (cycle - 1) + year - 1 + year >= 0
}
