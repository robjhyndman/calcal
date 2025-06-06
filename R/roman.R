# ==============================================================================
# Roman Calendar
# ==============================================================================

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
  # if (any(count < 1 | count > 19, na.rm = TRUE)) {
  #  stop("`count` must be between 1 and 19")
  # }
}

format_roman <- function(x, ...) {
  lst <- attributes(x)$calendar$from_rd(x)
  event <- c("Kalends", "Nones", "Ides")[lst$event]
  count <- lst$count
  prefix <- rep("ad", length(count))
  prefix[count == 2] <- "pridie"
  prefix[count == 1] <- ""
  days <- tolower(utils::as.roman(count))
  days[count <= 2] <- ""
  output <- trimws(paste(prefix, days, event))
  output <- gsub("  ", " ", output)
  output <- gsub(" ", "_", output)
  paste(lst$year, month.abb[lst$month], output, sep = "-")
}

fixed_from_roman <- function(date, ...) {
  kalends <- julian_date(date$year, date$month, 1) # KALENDS
  nones <- julian_date(date$year, date$month, nones_of_month(date$month)) # NONES
  ides <- julian_date(date$year, date$month, ides_of_month(date$month)) # IDES
  base_date <- kalends
  base_date[date$event == 2] <- nones[date$event == 2]
  base_date[date$event == 3] <- ides[date$event == 3]

  base_date -
    date$count +
    as.numeric(
      !(julian_leap_year(date$year) &
        date$month == MARCH &
        date$event == KALENDS &
        date$count >= 6 &
        date$count <= 16)
    ) +
    as.numeric(date$leap)
}

roman_from_fixed <- function(date, ...) {
  j_date <- cal_julian$from_rd(date)
  month <- j_date$month
  day <- j_date$day
  year <- j_date$year
  month_prime <- amod(month + 1, 12)
  year_prime <- year + (month_prime == 1 & year != -1)
  year_prime[month_prime == 1 & year == -1] <- 1
  kalends1 <- roman_date(year_prime, month_prime, KALENDS, 1, FALSE)
  case1 <- day == 1
  case2 <- !case1 & day <= nones_of_month(month)
  case3 <- !case1 & !case2 & day <= ides_of_month(month)
  case4 <- !case1 & !case2 & !case3 & (month != 2 | !julian_leap_year(year))
  case5 <- !case1 & !case2 & !case3 & !case4 & day < 25
  case6 <- !case1 & !case2 & !case3 & !case4 & !case5 & day == 25
  mmonth <- rep(3, length(date))
  event <- rep(KALENDS, length(date))
  count <- 31 - day
  leap <- day == 25
  leap[case1 | case2 | case3 | case4 | case5] <- FALSE
  if (any(case1)) {
    count[case1] <- 1
    mmonth[case1] <- month[case1]
  }
  if (any(case2)) {
    mmonth[case2] <- month[case2]
    event[case2] <- NONES
    count[case2] <- nones_of_month(month[case2]) - day[case2] + 1
  }
  if (any(case3)) {
    mmonth[case3] <- month[case3]
    event[case3] <- IDES
    count[case3] <- ides_of_month(month[case3]) - day[case3] + 1
  }
  if (any(case4)) {
    year[case4] <- year_prime[case4]
    mmonth[case4] <- month_prime[case4]
    count[case4] <- kalends1[case4] - date[case4] + 1
  }
  if (any(case5)) {
    count[case5] <- 30 - day[case5]
  }
  if (any(case6)) {
    count[case6] <- 31 - day[case6]
  }
  list(year = year, month = mmonth, event = event, count = count, leap = leap)
}

#' Work with Roman calendar dates
#'
#' @rdname roman
#' @examples
#' roman_date(66, 4, 1, 1, FALSE)
#' new_date(year = 66, month = 4, event = 1, count = 1, leap = FALSE, calendar = cal_roman)
#' @export
cal_roman <- cal_calendar(
  name = "Roman",
  short_name = "Rm",
  epoch = 0, # TO REPLACE,
  granularities = c("year", "month", "event", "count", "leap"),
  check_granularities = check_roman,
  format = format_roman,
  from_rd = roman_from_fixed,
  to_rd = fixed_from_roman
)

#' @rdname roman
#' @param year A numeric vector of years
#' @param month A numeric vector of months
#' @param event A numeric vector of events: 1 = KALENDS, 2 = NONES, 3 = IDES
#' @param count A numeric vector of counts:
#' @param leap A logical vector indicating if year is a leap year
#' @return A roman vector object
#' @export
roman_date <- function(
  year = integer(),
  month = integer(),
  event = integer(),
  count = integer(),
  leap = logical()
) {
  new_date(
    year = year,
    month = month,
    event = event,
    count = count,
    leap = leap,
    calendar = cal_roman
  )
}

#' @rdname roman
#' @param date Vector of dates on some calendar
#' @examples
#' as_roman("2016-01-01")
#' tibble::tibble(
#'   x = seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"),
#'   y = as_roman(x)
#' )
#' @export
as_roman <- function(date) {
  as_date(date, calendar = cal_roman)
}

ides_of_month <- function(month) {
  13 + 2 * (month %in% c(MARCH, MAY, JULY, OCTOBER))
}

nones_of_month <- function(month) {
  ides_of_month(month) - 8
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
