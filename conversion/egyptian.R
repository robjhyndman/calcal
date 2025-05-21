#==============================================================================
# Egyptian/Armenian Calendars
#==============================================================================

egyptian_date <- function(year, month, day) {
  c(year, month, day)
}

EGYPTIAN_EPOCH <- fixed_from_jd(1448638) # February 26, 747 BCE (Julian)

fixed_from_egyptian <- function(e_date) {
  month <- standard_month(e_date)
  day <- standard_day(e_date)
  year <- standard_year(e_date)

  EGYPTIAN_EPOCH -
    1 + # Days before start of calendar
    365 * (year - 1) + # Days in prior years
    30 * (month - 1) + # Days in prior months this year
    day # Days so far this month
}

egyptian_from_fixed <- function(date) {
  days <- date - EGYPTIAN_EPOCH # Elapsed days since epoch
  year <- 1 + days %/% 365 # Year since epoch
  month <- 1 + (days %% 365) %/% 30 # Calculate month by division
  day <- days - # Calculate day by subtraction
    365 * (year - 1) -
    30 * (month - 1) +
    1

  egyptian_date(year, month, day)
}

armenian_date <- function(year, month, day) {
  c(year, month, day)
}

ARMENIAN_EPOCH <- rd(201443) # July 11, 552 CE (Julian)

fixed_from_armenian <- function(a_date) {
  month <- standard_month(a_date)
  day <- standard_day(a_date)
  year <- standard_year(a_date)

  ARMENIAN_EPOCH +
    (fixed_from_egyptian(egyptian_date(year, month, day)) - EGYPTIAN_EPOCH)
}

armenian_from_fixed <- function(date) {
  egyptian_from_fixed(date + (EGYPTIAN_EPOCH - ARMENIAN_EPOCH))
}
