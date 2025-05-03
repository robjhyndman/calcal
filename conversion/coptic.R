#==============================================================================
# Coptic and Ethiopic Calendars
#==============================================================================

coptic_date <- function(year, month, day) {
  c(year, month, day)
}

COPTIC_EPOCH <- fixed_from_julian(julian_date(ce(284), AUGUST, 29))

coptic_leap_year <- function(c_year) {
  c_year %% 4 == 3
}

fixed_from_coptic <- function(c_date) {
  month <- standard_month(c_date)
  day <- standard_day(c_date)
  year <- standard_year(c_date)
  
  COPTIC_EPOCH - 1 +
  365 * (year - 1) +
  year %/% 4 +
  30 * (month - 1) +
  day
}

coptic_from_fixed <- function(date) {
  year <- (4 * (date - COPTIC_EPOCH) + 1463) %/% 1461
  month <- 1 + (date - fixed_from_coptic(coptic_date(year, 1, 1))) %/% 30
  day <- date + 1 - fixed_from_coptic(coptic_date(year, month, 1))
  
  coptic_date(year, month, day)
}

ethiopic_date <- function(year, month, day) {
  c(year, month, day)
}

ETHIOPIC_EPOCH <- fixed_from_julian(julian_date(ce(8), AUGUST, 29))

fixed_from_ethiopic <- function(e_date) {
  month <- standard_month(e_date)
  day <- standard_day(e_date)
  year <- standard_year(e_date)
  
  ETHIOPIC_EPOCH +
  (fixed_from_coptic(coptic_date(year, month, day)) - COPTIC_EPOCH)
}

ethiopic_from_fixed <- function(date) {
  coptic_from_fixed(date + (COPTIC_EPOCH - ETHIOPIC_EPOCH))
}

coptic_in_gregorian <- function(c_month, c_day, g_year) {
  jan1 <- gregorian_new_year(g_year)
  y <- standard_year(coptic_from_fixed(jan1))
  
  date0 <- fixed_from_coptic(coptic_date(y, c_month, c_day))
  date1 <- fixed_from_coptic(coptic_date(y + 1, c_month, c_day))
  
  list_range(c(date0, date1), gregorian_year_range(g_year))
}

coptic_christmas <- function(g_year) {
  coptic_in_gregorian(4, 29, g_year)
}
