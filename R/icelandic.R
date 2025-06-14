#==============================================================================
# Icelandic Calendar
#==============================================================================

ICELANDIC_EPOCH <- 109 # as.numeric(gregorian_date(1, APRIL, 19))
iSUMMER <- 1
iWINTER <- 2

fixed_from_icelandic <- function(i_date) {
  start <- rep(0,length(date))
  start[i_date$season == iSUMMER] <- icelandic_summer(i_date$year)
  start[i_date$season != iSUMMER] <- icelandic_winter(i_date$year)
  
  shift <- rep(SATURDAY, length(date)) - 2* (i_date$season == iSUMMER)
  start + 7 * (i_date$week - 1) + (i_date$weekday - shift) %% 7
}

icelandic_from_fixed <- function(date) {
  date <- vec_data(date)
  approx <- (date - ICELANDIC_EPOCH + 369) %/% (146097 / 400)
  year <- approx - (date < icelandic_summer(approx)) 
  season <- iWINTER - (date < icelandic_winter(year)) 
  start <- rep(0, length(date[[1]]))
  start[season == iSUMMER] <- icelandic_summer(year[season == iSUMMER])
  start[season == iWINTER] <- icelandic_winter(year[season == iWINTER])
  week <- 1 + (date - start) %/% 7
  weekday <- day_of_week_from_fixed(date)

  list(year = year, season=season, week=week, weekday=weekday)
}

check_icelandic <- function(date) {}

#' @rdname cal_calendar
#' @format NULL
#' @export
cal_icelandic <- cal_calendar(
  "icelandic",
  "Ice",
  c("year", "season", "week","weekday"),
  check_icelandic,
  format_date,
  icelandic_from_fixed,
  fixed_from_icelandic
)

#' Icelandic dates
#' 
#' @param year A numeric vector of years
#' @param season A numeric vector of seasons (1 = Summer, 2 = Winter)
#' @param week A numeric vector of weeks within the season
#' @param weekday A number vector containing day of week
#' @examples
#' gregorian_date(2025, 4, 20:30) |>
#'   as_icelandic()
#' @export
icelandic_date <- function(year, season, week, weekday) {
  new_date(year = year, season = season, week=week, weekday= weekday,
  calendar = cal_icelandic)
}

#' @rdname icelandic_date
#' @param date A numeric vector of dates
#' @export
as_icelandic <- function(date) {
  as_date(date, calendar = cal_icelandic)
}


icelandic_summer <- function(i_year) {
  apr19 <- ICELANDIC_EPOCH +
    365 * (i_year - 1) +
    (i_year %/% 4 - i_year %/% 100 + i_year %/% 400)

  kday_on_or_after(THURSDAY, apr19)
}

icelandic_winter <- function(i_year) {
  icelandic_summer(i_year + 1) - 180
}


icelandic_leap_year <- function(i_year) {
  (icelandic_summer(i_year + 1) - icelandic_summer(i_year)) != 364
}
