#==============================================================================
# Islamic Calendar
#==============================================================================

islamic_date <- function(year, month, day) {
  c(year, month, day)
}

ISLAMIC_EPOCH <- fixed_from_julian(julian_date(ce(622), JULY, 16))

islamic_leap_year <- function(i_year) {
  (i_year * 11 + 14) %% 30 < 11
}

fixed_from_islamic <- function(i_date) {
  month <- standard_month(i_date)
  day <- standard_day(i_date)
  year <- standard_year(i_date)
  
  ISLAMIC_EPOCH - 1 +
  354 * (year - 1) +
  (3 + 11 * year) %/% 30 +
  29 * (month - 1) +
  month %/% 2 +
  day
}

islamic_from_fixed <- function(date) {
  year <- (30 * (date - ISLAMIC_EPOCH) + 10646) %/% 10631
  prior_days <- date - fixed_from_islamic(islamic_date(year, 1, 1))
  month <- (11 * prior_days + 330) %/% 325
  day <- date - fixed_from_islamic(islamic_date(year, month, 1)) + 1
  
  islamic_date(year, month, day)
}

islamic_in_gregorian <- function(i_month, i_day, g_year) {
  jan1 <- gregorian_new_year(g_year)
  y <- standard_year(islamic_from_fixed(jan1))
  
  date0 <- fixed_from_islamic(islamic_date(y, i_month, i_day))
  date1 <- fixed_from_islamic(islamic_date(y + 1, i_month, i_day))
  date2 <- fixed_from_islamic(islamic_date(y + 2, i_month, i_day))
  
  list_range(c(date0, date1, date2), gregorian_year_range(g_year))
}

mawlid <- function(g_year) {
  islamic_in_gregorian(3, 12, g_year)
}
