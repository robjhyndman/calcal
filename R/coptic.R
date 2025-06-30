#==============================================================================
# Coptic and Ethiopic Calendars
#==============================================================================

COPTIC_EPOCH <- 103605 # as.numeric(julian_date(284, AUGUST, 29))
ETHIOPIC_EPOCH <- 2796 # as.numeric(julian_date(8, AUGUST, 29))

validate_coptic <- function(date) {
  if (any(date$month < 1 | date$month > 13)) {
    stop("month must be between 1 and 13")
  }
  if (any(date$day < 1 | date$day > 30)) {
    stop("day must be between 1 and 30")
  }
}

fixed_from_coptic <- function(date) {
  COPTIC_EPOCH -
    1 +
    365 * (date$year - 1) +
    date$year %/% 4 +
    30 * (date$month - 1) +
    date$day
}

coptic_from_fixed <- function(date) {
  date <- vec_data(date)
  year <- (4 * (date - COPTIC_EPOCH) + 1463) %/% 1461
  month <- 1 +
    (date - fixed_from_coptic(list(year = year, month = 1, day = 1))) %/% 30
  day <- date + 1 - fixed_from_coptic(list(year = year, month = month, day = 1))

  list(year = year, month = month, day = day)
}

format_coptic <- function(date) {
  format_date(
    date,
    month_name = c(
      "Tho",
      "Pao",
      "Ath",
      "Koi",
      "Tob",
      "Mesh",
      "Parem",
      "Parm",
      "Pash",
      "Pao",
      "Epep",
      "Mes",
      "Epa"
    )
  )
}

#' @rdname cal_calendar
#' @format NULL
#' @export
cal_coptic <- cal_calendar(
  name = "coptic",
  short_name = "Cop",
  granularities = c("year", "month", "day"),
  validate_granularities = validate_coptic,
  format = format_coptic,
  from_rd = coptic_from_fixed,
  to_rd = fixed_from_coptic
)

fixed_from_ethiopic <- function(date) {
  ETHIOPIC_EPOCH +
    (vec_data(coptic_date(date$year, date$month, date$day)) -
      COPTIC_EPOCH)
}

ethiopic_from_fixed <- function(date) {
  coptic_from_fixed(date + (COPTIC_EPOCH - ETHIOPIC_EPOCH))
}

format_ethiopic <- function(date) {
  format_date(
    date,
    month_name = c(
      "Mas",
      "Teq",
      "Hed",
      "Takh",
      "Ter",
      "Yak",
      "Mag",
      "Miy",
      "Gen",
      "Sane",
      "Ham",
      "Nah",
      "Pag"
    )
  )
}

#' @rdname cal_calendar
#' @format NULL
#' @export
cal_ethiopic <- cal_calendar(
  name = "ethiopic",
  short_name = "Eth",
  granularities = c("year", "month", "day"),
  validate_granularities = validate_coptic,
  format = format_ethiopic,
  from_rd = ethiopic_from_fixed,
  to_rd = fixed_from_ethiopic
)

#' Coptic and Ethoiopic dates
#'
#' These two calendars are identical apart from the starting point or epoch.
#' The Coptic calendar (also called the Alexandrian calendar) starts on
#' 29 August 284 CE in the Julian calendar, while the Ethiopic (or Ethiopian)
#' calendar starts on 29 August 8 CE in the Julian calendar.
#' The Coptic calendar is used by the Coptic Orthodox and Coptic Catholic Churches,
#' while the Ethiopic calendar is the official state calendar of Ethiopia, and
#' unofficial calendar of Eritrea, and is
#' used by the Ethiopian and Eritrean Orthodox Churches. Both calendars have 13
#' months, with 12 months of 30 days and a 13th month of 5 or 6 days depending
#' on whether it is a leap year. Leap years occur every 4 years.
#'
#' @rdname coptic
#' @param year A numeric vector of years
#' @param month A numeric vector of months
#' @param day A numeric vector of days
#' @return A coptic vector object
#' @seealso [cal_coptic], [cal_ethiopic], [coptic_christmas]
#' @examples
#' tibble::tibble(
#'   gregorian = gregorian_date(2025, 1, 1:31),
#'   coptic = as_coptic(gregorian),
#'   ethiopic = as_ethiopic(gregorian)
#' )
#' coptic_date(1741, 5, 16:18)
#' as_date(Sys.Date(), calendar = cal_ethiopic)
#' as_coptic("2016-01-01")
#' as_ethiopic(Sys.Date())
#' @export
coptic_date <- function(
  year = integer(),
  month = integer(),
  day = integer()
) {
  new_date(
    year = year,
    month = month,
    day = day,
    calendar = cal_coptic
  )
}

#' @rdname coptic
#' @export
ethiopic_date <- function(
  year = integer(),
  month = integer(),
  day = integer()
) {
  new_date(
    year = year,
    month = month,
    day = day,
    calendar = cal_ethiopic
  )
}
#' @rdname coptic
#' @param date A numeric vector of dates
#' @export
as_coptic <- function(date) {
  as_date(date, calendar = cal_coptic)
}

#' @rdname coptic
#' @param date A numeric vector of dates
#' @export
as_ethiopic <- function(date) {
  as_date(date, calendar = cal_ethiopic)
}

coptic_leap_year <- function(c_year) {
  c_year %% 4 == 3
}

coptic_in_gregorian <- function(c_month, c_day, g_year) {
  jan1 <- gregorian_new_year(g_year)
  y <- coptic_from_fixed(jan1)$year
  date0 <- fixed_from_coptic(list(year = y, month = c_month, day = c_day))
  date1 <- fixed_from_coptic(list(year = y + 1, month = c_month, day = c_day))
  dates2_in_gregorian(g_year, date0, date1)
}

#' Coptic Christmas
#'
#' Coptic Christmas is celebrated on 29th of Koiak in the Coptic calendar, which
#' currently corresponds to 7 or 8 January in the Gregorian calendar.
#' @rdname christian
#' @export
coptic_christmas <- function(year) {
  coptic_in_gregorian(4, 29, year) |> as_gregorian()
}

#' @export
day_of_week.coptic <- function(date, ...) {
  dow <- day_of_week_from_fixed(date) + 1
  c(
    "Tkyriake",
    "Pesnau",
    "Pshoment",
    "Peftoou",
    "Ptiou",
    "Psoou",
    "Psabbaton"
  )[dow]
}

#' @export
day_of_week.ethiopic <- function(date, ...) {
  dow <- day_of_week_from_fixed(date) + 1
  c(
    "Ihud",
    "Sanyo",
    "Maksanyo",
    "Rob",
    "Hamus",
    "Arb",
    "Kidamme"
  )[dow]
}
