#==============================================================================
# Icelandic Calendar
#==============================================================================

ICELANDIC_EPOCH <- 109 # as.numeric(gregorian_date(1, APRIL, 19))
iSUMMER <- 1
iWINTER <- 2

fixed_from_icelandic <- function(i_date) {
  start <- rep(0, length(date))
  start[i_date$season == iSUMMER] <- icelandic_summer(i_date$year)
  start[i_date$season != iSUMMER] <- icelandic_winter(i_date$year)

  shift <- rep(SATURDAY, length(date)) - 2 * (i_date$season == iSUMMER)
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

  list(year = year, season = season, week = week, weekday = weekday)
}

format_icelandic <- function(date) {
  parts <- base_granularities(date)
  summer <- parts[["season"]] == iSUMMER
  parts[["season"]] <- rep("Sum", length(parts[["season"]]))
  parts[["season"]][!summer] <- "Win"
  parts[["weekday"]] <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[
    parts[["weekday"]] + 1
  ]
  for (i in seq_along(parts)) {
    if (is.numeric(parts[[i]])) {
      parts[[i]] <- sprintf("%.2d", parts[[i]])
    }
  }
  apply(as.data.frame(parts), 1, function(x) {
    paste(x, collapse = "-")
  })
}

validate_icelandic <- function(date) {
  if (any(date$season != iSUMMER & date$season != iWINTER)) {
    stop("Two seasons are allowed (1 = Summer and 2 = Winter)")
  }
  if (any(date$weekday < 0 | date$weekday > 6)) {
    stop("Weekdays must be between 0 (Sunday) and 6 (Saturday)")
  }
  if (any(date$week < 1 | date$week > 28)) {
    stop("Weeks must be between 1 and 28")
  }
  if (any(date$season == iWINTER & date$week > 26)) {
    stop("Winter weeks must be between 1 and 26")
  }
}

#' @rdname cal_calendar
#' @format NULL
#' @export
cal_icelandic <- cal_calendar(
  "icelandic",
  "Ice",
  c("year", "season", "week", "weekday"),
  validate_icelandic,
  format_icelandic,
  icelandic_from_fixed,
  fixed_from_icelandic
)

#' Icelandic calendar dates
#' 
#' The Icelandic calendar, still in use in Iceland, divides times into 
#' 7-day weeks and two seasons: Summer and Winter. Summer starts on the first
#' Thursday after April 18th, and Winter 180 days earlier. Ordinary years have 
#' 52 weeks with leap years having 53 weeks. The leap week occurs every 5-7 
#' years in midsummer.
#'
#' @param year A numeric vector of years
#' @param season A numeric vector of seasons (1 = Summer, 2 = Winter)
#' @param week A numeric vector of weeks within the season (1 to 28)
#' @param weekday A number vector containing day of week (0 = Sunday, 1 = Monday,
#' ..., 6 = Saturday))
#' @examples
#' gregorian_date(2025, 4, 20:30) |>
#'   as_icelandic()
#' icelandic_date(2025, 1, 6, 0:6) |>
#'   day_of_week()
#' @export
icelandic_date <- function(
  year = integer(),
  season = integer(),
  week = integer(),
  weekday = integer()
) {
  new_date(
    year = year,
    season = season,
    week = week,
    weekday = weekday,
    calendar = cal_icelandic
  )
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

#' @export
day_of_week.icelandic <- function(date, ...) {
  dow <- day_of_week_from_fixed(date) + 1
  c(
    "Sunnudagur",
    "Manudagur",
    "\u00deri\u00f0judagur",
    "Mi\u00f0vikudagur",
    "Fimmtudagur",
    "F\u00f8studagur",
    "Laugardagur"
  )[dow]
}
