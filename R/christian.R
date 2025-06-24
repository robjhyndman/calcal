# ==============================================================================
# Section: Ecclesiastical Calendars
# ==============================================================================

#' Christian Ecclesiastical Holidays
#'
#' Functions to return Gregorian dates for various Christian ecclesiastical holidays and other special days
#'
#' @param year Gregorian year
#'
#' @rdname christian
#' @examples
#' tibble::tibble(
#'   year = 2025:2030,
#'   advent = advent(year),
#'   christmas = christmas(year),
#'   orthodox_christmas = orthodox_christmas(year),
#'   epiphany = epiphany(year),
#'   easter = easter(year),
#'   orthodox_easter = orthodox_easter(year),
#'   pentecost = pentecost(year)
#' )
#' @export
advent <- function(year) {
  # Fixed date of Advent in Gregorian year--the Sunday closest to November 30
  as_gregorian(kday_nearest(SUNDAY, gregorian_date(year, NOVEMBER, 30)))
}

#' @rdname christian
#' @export
christmas <- function(year) {
  # Fixed date of Christmas in Gregorian year
  gregorian_date(year, DECEMBER, 25)
}

#' @rdname christian
#' @export
orthodox_christmas <- function(year) {
  # List of zero or one fixed dates of Eastern Orthodox Christmas in Gregorian year
  as_gregorian(julian_in_gregorian(DECEMBER, 25, year))
}

#' @rdname christian
#' @export
epiphany <- function(year) {
  # Fixed date of Epiphany in U.S. in Gregorian year--the first Sunday after January 1
  as_gregorian(first_kday(SUNDAY, gregorian_date(year, JANUARY, 2)))
}

#' @rdname christian
#' @export
easter <- function(year) {
  # Fixed date of Easter in Gregorian year
  century <- 1 + year %/% 100

  # Age of moon for April 5 by Nicaean rule, corrected for the Gregorian century rule
  # and for Metonic cycle inaccuracy
  shifted_epact <- (14 +
    11 * (year %% 19) -
    (3 * century) %/% 4 +
    (5 + 8 * century) %/% 25) %%
    30

  # Adjust for 29.5 day month
  adjusted_epact <- shifted_epact +
    (shifted_epact == 0 | (shifted_epact == 1 & (year %% 19) > 10))

  # Day after full moon on or after March 21
  paschal_moon <- gregorian_date(year, APRIL, 19) - adjusted_epact

  # Return the Sunday following the Paschal moon
  as_gregorian(kday_after(SUNDAY, paschal_moon))
}

#' @rdname christian
#' @export
orthodox_easter <- function(year) {
  # Fixed date of Orthodox Easter in Gregorian year
  shifted_epact <- (14 + 11 * (year %% 19)) %% 30 # Age of moon for April 5
  j_year <- year - (year <= 0) # Julian year number

  # Day after full moon on or after March 21
  paschal_moon <- julian_date(j_year, APRIL, 19) - shifted_epact

  # Return the Sunday following the Paschal moon
  as_gregorian(kday_after(SUNDAY, paschal_moon))
}

#' @rdname christian
#' @export
pentecost <- function(year) {
  # Fixed date of Pentecost in Gregorian year
  easter(year) + 49
}
