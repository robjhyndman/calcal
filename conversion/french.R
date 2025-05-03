#==============================================================================
# French Revolutionary Calendar
#==============================================================================

french_date <- function(year, month, day) {
  c(year, month, day)
}

FRENCH_EPOCH <- fixed_from_gregorian(gregorian_date(1792, SEPTEMBER, 22))

PARIS <- location(angle(48, 50, 11), angle(2, 20, 15), mt(27), hr(1))

midnight_in_paris <- function(date) {
  midnight(date + 1, PARIS)
}

french_new_year_on_or_before <- function(date) {
  approx <- estimate_prior_solar_longitude(AUTUMN, midnight_in_paris(date))
  
  next_value(floor(approx) - 1, function(day) {
    AUTUMN <= solar_longitude(midnight_in_paris(day))
  })
}

fixed_from_french <- function(f_date) {
  month <- standard_month(f_date)
  day <- standard_day(f_date)
  year <- standard_year(f_date)
  
  new_year <- french_new_year_on_or_before(
    floor(FRENCH_EPOCH + 180 + MEAN_TROPICAL_YEAR * (year - 1))
  )
  
  new_year - 1 +        # Days in prior years
  30 * (month - 1) +    # Days in prior months
  day                   # Days this month
}

french_from_fixed <- function(date) {
  new_year <- french_new_year_on_or_before(date)
  year <- 1 + round((new_year - FRENCH_EPOCH) / MEAN_TROPICAL_YEAR)
  month <- 1 + (date - new_year) %/% 30
  day <- 1 + (date - new_year) %% 30
  
  french_date(year, month, day)
}

french_leap_year <- function(f_year) {
  fixed_from_french(french_date(f_year + 1, 1, 1)) - 
  fixed_from_french(french_date(f_year, 1, 1)) > 365
}

arithmetic_french_leap_year <- function(f_year) {
  (f_year %% 4 == 0) && 
  !(f_year %% 400 %in% c(100, 200, 300)) && 
  !(f_year %% 4000 == 0)
}

fixed_from_arithmetic_french <- function(f_date) {
  month <- standard_month(f_date)
  day <- standard_day(f_date)
  year <- standard_year(f_date)
  
  FRENCH_EPOCH - 1 +                    # Days before start of calendar
  365 * (year - 1) +                    # Ordinary days in prior years
  (year - 1) %/% 4 -                    # Leap days in prior years
  (year - 1) %/% 100 +                  # Subtract century years
  (year - 1) %/% 400 -                  # Add 400-year cycles
  (year - 1) %/% 4000 +                 # Subtract 4000-year cycles
  30 * (month - 1) +                    # Days in prior months this year
  day                                   # Days this month
}

arithmetic_french_from_fixed <- function(date) {
  approx <- 1 + (date - FRENCH_EPOCH + 2) %/% (1460969/4000)
  year <- if (date < fixed_from_arithmetic_french(french_date(approx, 1, 1))) {
    approx - 1
  } else {
    approx
  }
  
  month <- 1 + (date - fixed_from_arithmetic_french(french_date(year, 1, 1))) %/% 30
  day <- 1 + date - fixed_from_arithmetic_french(french_date(year, month, 1))
  
  french_date(year, month, day)
}
