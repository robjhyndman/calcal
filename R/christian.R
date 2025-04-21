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
#' @export
christmas <- function(year) {
  # Fixed date of Christmas in Gregorian year
  gregorian(year, DECEMBER, 25)
}

#' @rdname christian
#' @export
advent <- function(year) {
  # Fixed date of Advent in Gregorian year--the Sunday closest to November 30
  as_gregorian(kday_nearest(
    as_rd(gregorian(year, NOVEMBER, 30)),
    SUNDAY
  ))
}

#' @rdname christian
#' @export
epiphany <- function(year) {
  # Fixed date of Epiphany in U.S. in Gregorian year--the first Sunday after January 1
  as_gregorian(first_kday(SUNDAY, gregorian(year, JANUARY, 2)))
}

#' @rdname christian
#' @export
eastern_orthodox_christmas <- function(year) {
  # List of zero or one fixed dates of Eastern Orthodox Christmas in Gregorian year
  julian_in_gregorian(DECEMBER, 25, year)
}

#' @rdname christian
#' @export
orthodox_easter <- function(year) {
  # Fixed date of Orthodox Easter in Gregorian year
  shifted_epact <- (14 + 11 * (year %% 19)) %% 30 # Age of moon for April 5
  j_year <- year - (year <= 0) # Julian year number

  # Day after full moon on or after March 21
  paschal_moon <- as_rd(julian(j_year, APRIL, 19)) - shifted_epact

  # Return the Sunday following the Paschal moon
  as_gregorian(kday_after(paschal_moon, SUNDAY))
}

#' @rdname christian
#' @export
alt_orthodox_easter <- function(year) {
  # Alternate calculation of fixed date of Orthodox Easter in Gregorian year
  # Day after full moon on or after March 21
  paschal_moon <- 354 *
    year +
    30 * (7 * year + 8) %/% 19 +
    year %/% 4 -
    year %/% 19 -
    272

  # Return the Sunday following the Paschal moon
  as_gregorian(kday_after(paschal_moon, SUNDAY))
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
  paschal_moon <- as_rd(gregorian(year, APRIL, 19)) - adjusted_epact

  # Return the Sunday following the Paschal moon
  as_gregorian(kday_after(paschal_moon, SUNDAY))
}

#' @rdname christian
#' @export
pentecost <- function(year) {
  # Fixed date of Pentecost in Gregorian year
  easter(year) + 49
}
