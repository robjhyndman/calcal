# Astronomical Lunar Calendars

# Babylonian date structure and accessors
babylonian_date <- function(year, month, leap, day) {
  list(year = year, month = month, leap = leap, day = day)
}

babylonian_year <- function(date) {
  date[[1]]
}

babylonian_month <- function(date) {
  date[[2]]
}

babylonian_leap <- function(date) {
  date[[3]]
}

babylonian_day <- function(date) {
  date[[4]]
}

# Location and epoch constants
babylon <- location(deg(32.4794), deg(44.4328), mt(26), hr(3 + 1/2))

babylonian_epoch <- fixed_from_julian(julian_date(bce(311), april, 3))

islamic_location <- location(deg(30.1), deg(31.3), mt(200), hr(2))

jerusalem <- location(deg(31.78), deg(35.24), mt(740), hr(2))

acre <- location(deg(32.94), deg(35.09), mt(22), hr(2))

hebrew_location <- location(deg(32.82), deg(35), mt(0), hr(2))

samaritan_location <- location(deg(32.1994), deg(35.2728), mt(881), hr(2))

samaritan_epoch <- fixed_from_julian(julian_date(bce(1639), march, 15))

# Utility functions
moonlag <- function(date, location) {
  # Time between sunset and moonset on date at location
  # Returns bogus if there is no sunset on date
  sun <- sunset(date, location)
  moon <- moonset(date, location)
  
  if (sun == bogus) {
    bogus
  } else if (moon == bogus) {
    hr(24)  # Arbitrary
  } else {
    moon - sun
  }
}

babylonian_leap_year_p <- function(b_year) {
  # True if b_year is a leap year on Babylonian calendar
  ((7 * b_year + 13) %% 19) < 7
}

babylonian_criterion <- function(date) {
  # Moonlag criterion for visibility of crescent moon on 
  # eve of date in Babylon
  set <- sunset(date - 1, babylon)
  tee <- universal_from_standard(set, babylon)
  phase <- lunar_phase(tee)
  
  (new < phase && phase < first_quarter) &&
  (new_moon_before(tee) <= (tee - hr(24))) &&
  (moonlag(date - 1, babylon) > mn(48))
}

babylonian_new_month_on_or_before <- function(date) {
  # Fixed date of start of Babylonian month on or before
  # Babylonian date. Using lag of moonset criterion
  moon <- fixed_from_moment(lunar_phase_at_or_before(new, date))
  age <- date - moon
  
  tau <- if (age <= 3 && !babylonian_criterion(date)) {
    moon - 30  # Must go back a month
  } else {
    moon
  }
  
  next_func(tau, babylonian_criterion)
}

fixed_from_babylonian <- function(b_date) {
  # Fixed date equivalent to Babylonian date
  month <- babylonian_month(b_date)
  leap <- babylonian_leap(b_date)
  day <- babylonian_day(b_date)
  year <- babylonian_year(b_date)
  
  # Elapsed months this year
  month1 <- if (leap || ((year %% 19) == 18 && month > 6)) {
    month
  } else {
    month - 1
  }
  
  # Elapsed months since epoch
  months <- (((year - 1) * 235 + 13) %/% 19) + month1
  
  # Middle of given month
  midmonth <- babylonian_epoch + round(mean_synodic_month * months) + 15
  
  babylonian_new_month_on_or_before(midmonth) + day - 1
}

babylonian_from_fixed <- function(date) {
  # Babylonian date corresponding to fixed date
  crescent <- babylonian_new_month_on_or_before(date)
  months <- round((crescent - babylonian_epoch) / mean_synodic_month)
  year <- 1 + ((19 * months + 5) %/% 235)
  
  approx <- babylonian_epoch + 
            round(((((year - 1) * 235 + 13) %/% 19) * mean_synodic_month))
  new_year <- babylonian_new_month_on_or_before(approx + 15)
  month1 <- 1 + round((crescent - new_year) / 29.5)
  special <- (year %% 19) == 18
  leap <- if (special) (month1 == 7) else (month1 == 13)
  month <- if (leap || (special && month1 > 6)) {
    month1 - 1
  } else {
    month1
  }
  day <- date - crescent + 1
  
  babylonian_date(year, month, leap, day)
}

phasis_on_or_before <- function(date, location) {
  # Closest fixed date on or before date when crescent
  # moon first became visible at location
  moon <- fixed_from_moment(lunar_phase_at_or_before(new, date))
  age <- date - moon
  
  tau <- if (age <= 3 && !visible_crescent(date, location)) {
    moon - 30  # Must go back a month
  } else {
    moon
  }
  
  next_func(tau, function(d) visible_crescent(d, location))
}

phasis_on_or_after <- function(date, location) {
  # Closest fixed date on or after date on the eve
  # of which crescent moon first became visible at location
  moon <- fixed_from_moment(lunar_phase_at_or_before(new, date))
  age <- date - moon
  
  tau <- if (4 <= age || visible_crescent(date - 1, location)) {
    moon + 29  # Next new moon
  } else {
    date
  }
  
  next_func(tau, function(d) visible_crescent(d, location))
}

fixed_from_observational_islamic <- function(i_date) {
  # Fixed date equivalent to Observational Islamic date
  month <- standard_month(i_date)
  day <- standard_day(i_date)
  year <- standard_year(i_date)
  
  # Middle of given month
  midmonth <- islamic_epoch + 
              floor(((year - 1) * 12 + month - 1/2) * mean_synodic_month)
  
  phasis_on_or_before(midmonth, islamic_location) + day - 1
}

observational_islamic_from_fixed <- function(date) {
  # Observational Islamic date corresponding to fixed date
  crescent <- phasis_on_or_before(date, islamic_location)
  elapsed_months <- round((crescent - islamic_epoch) / mean_synodic_month)
  year <- 1 + (elapsed_months %/% 12)
  month <- 1 + (elapsed_months %% 12)
  day <- 1 + (date - crescent)
  
  islamic_date(year, month, day)
}

astronomical_easter <- function(g_year) {
  # Date of (proposed) astronomical Easter in Gregorian year
  equinox <- season_in_gregorian(spring, g_year)
  paschal_moon <- floor(apparent_from_universal(
    lunar_phase_at_or_after(full, equinox), jerusalem))
  
  # Return the Sunday following the Paschal moon
  kday_after(sunday, paschal_moon)
}

saudi_criterion <- function(date) {
  # Saudi visibility criterion on eve of fixed date in Mecca
  set <- sunset(date - 1, mecca)
  tee <- universal_from_standard(set, mecca)
  phase <- lunar_phase(tee)
  
  (new < phase && phase < first_quarter) &&
  (moonlag(date - 1, mecca) > 0)
}

saudi_new_month_on_or_before <- function(date) {
  # Closest fixed date on or before date when Saudi
  # visibility criterion held
  moon <- fixed_from_moment(lunar_phase_at_or_before(new, date))
  age <- date - moon
  
  tau <- if (age <= 3 && !saudi_criterion(date)) {
    moon - 30  # Must go back a month
  } else {
    moon
  }
  
  next_func(tau, saudi_criterion)
}

fixed_from_saudi_islamic <- function(s_date) {
  # Fixed date equivalent to Saudi Islamic date
  month <- standard_month(s_date)
  day <- standard_day(s_date)
  year <- standard_year(s_date)
  
  # Middle of given month
  midmonth <- islamic_epoch + 
              floor(((year - 1) * 12 + month - 1/2) * mean_synodic_month)
  
  saudi_new_month_on_or_before(midmonth) + day - 1
}

saudi_islamic_from_fixed <- function(date) {
  # Saudi Islamic date corresponding to fixed date
  crescent <- saudi_new_month_on_or_before(date)
  elapsed_months <- round((crescent - islamic_epoch) / mean_synodic_month)
  year <- 1 + (elapsed_months %/% 12)
  month <- 1 + (elapsed_months %% 12)
  day <- 1 + (date - crescent)
  
  islamic_date(year, month, day)
}

observational_hebrew_first_of_nisan <- function(g_year) {
  # Fixed date of Observational (classical) Nisan 1 occurring in Gregorian year
  equinox <- season_in_gregorian(spring, g_year)
  set <- universal_from_standard(
    sunset(floor(equinox), hebrew_location), hebrew_location)
  
  phasis_on_or_after(
    floor(equinox) - ifelse(equinox < set, 14, 13),
    hebrew_location)
}

fixed_from_observational_hebrew <- function(h_date) {
  # Fixed date equivalent to Observational Hebrew date
  month <- standard_month(h_date)
  day <- standard_day(h_date)
  year <- standard_year(h_date)
  year1 <- ifelse(month >= tishri, year - 1, year)
  
  start <- fixed_from_hebrew(hebrew_date(year1, nisan, 1))
  g_year <- gregorian_year_from_fixed(start + 60)
  new_year <- observational_hebrew_first_of_nisan(g_year)
  
  # Middle of given month
  midmonth <- new_year + round(29.5 * (month - 1)) + 15
  
  phasis_on_or_before(midmonth, hebrew_location) + day - 1
}

observational_hebrew_from_fixed <- function(date) {
  # Observational Hebrew date corresponding to fixed date
  crescent <- phasis_on_or_before(date, hebrew_location)
  g_year <- gregorian_year_from_fixed(date)
  ny <- observational_hebrew_first_of_nisan(g_year)
  
  new_year <- if (date < ny) {
    observational_hebrew_first_of_nisan(g_year - 1)
  } else {
    ny
  }
  
  month <- 1 + round((crescent - new_year) / 29.5)
  year <- standard_year(hebrew_from_fixed(new_year)) + 
          ifelse(month >= tishri, 1, 0)
  day <- date - crescent + 1
  
  hebrew_date(year, month, day)
}

month_length <- function(date, location) {
  # Length of lunar month based on observability at location
  moon <- phasis_on_or_after(date + 1, location)
  prev <- phasis_on_or_before(date, location)
  moon - prev
}

early_month_p <- function(date, location) {
  # Fixed date in location is in a month that was forced to start early
  start <- phasis_on_or_before(date, location)
  prev <- start - 15
  
  ((date - start) >= 30) ||
  (month_length(prev, location) > 30) ||
  (month_length(prev, location) == 30 && early_month_p(prev, location))
}

alt_fixed_from_observational_islamic <- function(i_date) {
  # Fixed date equivalent to Observational Islamic date
  # Months are never longer than 30 days
  month <- standard_month(i_date)
  day <- standard_day(i_date)
  year <- standard_year(i_date)
  
  # Middle of given month
  midmonth <- islamic_epoch + 
              floor(((year - 1) * 12 + month - 1/2) * mean_synodic_month)
  
  moon <- phasis_on_or_before(midmonth, islamic_location)
  date <- moon + day - 1
  
  ifelse(early_month_p(midmonth, islamic_location), date - 1, date)
}

alt_observational_islamic_from_fixed <- function(date) {
  # Observational Islamic date corresponding to fixed date
  # Months are never longer than 30 days
  early <- early_month_p(date, islamic_location)
  long <- early && (month_length(date, islamic_location) > 29)
  
  date_prime <- ifelse(long, date + 1, date)
  moon <- phasis_on_or_before(date_prime, islamic_location)
  elapsed_months <- round((moon - islamic_epoch) / mean_synodic_month)
  year <- 1 + (elapsed_months %/% 12)
  month <- 1 + (elapsed_months %% 12)
  day <- date_prime - moon + ifelse(early && !long, 2, 1)
  
  islamic_date(year, month, day)
}

alt_observational_hebrew_from_fixed <- function(date) {
  # Observational Hebrew date corresponding to fixed date
  # Months are never longer than 30 days
  early <- early_month_p(date, hebrew_location)
  long <- early && (month_length(date, hebrew_location) > 29)
  
  date_prime <- ifelse(long, date + 1, date)
  moon <- phasis_on_or_before(date_prime, hebrew_location)
  g_year <- gregorian_year_from_fixed(date_prime)
  ny <- observational_hebrew_first_of_nisan(g_year)
  
  new_year <- if (date_prime < ny) {
    observational_hebrew_first_of_nisan(g_year - 1)
  } else {
    ny
  }
  
  month <- 1 + round((moon - new_year) / 29.5)
  year <- standard_year(hebrew_from_fixed(new_year)) + 
          ifelse(month >= tishri, 1, 0)
  day <- date_prime - moon + ifelse(early && !long, 2, 1)
  
  hebrew_date(year, month, day)
}

alt_fixed_from_observational_hebrew <- function(h_date) {
  # Fixed date equivalent to Observational Hebrew date
  # Months are never longer than 30 days
  month <- standard_month(h_date)
  day <- standard_day(h_date)
  year <- standard_year(h_date)
  year1 <- ifelse(month >= tishri, year - 1, year)
  
  start <- fixed_from_hebrew(hebrew_date(year1, nisan, 1))
  g_year <- gregorian_year_from_fixed(start + 60)
  new_year <- observational_hebrew_first_of_nisan(g_year)
  
  # Middle of given month
  midmonth <- new_year + round(29.5 * (month - 1)) + 15
  moon <- phasis_on_or_before(midmonth, hebrew_location)
  date <- moon + day - 1
  
  ifelse(early_month_p(midmonth, hebrew_location), date - 1, date)
}

classical_passover_eve <- function(g_year) {
  # Fixed date of Classical (observational) Passover Eve
  # (Nisan 14) occurring in Gregorian year
  observational_hebrew_first_of_nisan(g_year) + 13
}

# Samaritan calendar functions
samaritan_noon <- function(date) {
  # Universal time of true noon on date at Samaritan location
  midday(date, samaritan_location)
}

samaritan_new_moon_after <- function(tee) {
  # Fixed date of first new moon after UT moment tee
  # Modern calculation
  ceiling(apparent_from_universal(new_moon_at_or_after(tee), 
                                  samaritan_location) - hr(12))
}

samaritan_new_moon_at_or_before <- function(tee) {
  # Fixed-date of last new moon before UT moment tee
  # Modern calculation
  ceiling(apparent_from_universal(new_moon_before(tee), 
                                  samaritan_location) - hr(12))
}

samaritan_new_year_on_or_before <- function(date) {
  # Fixed date of Samaritan New Year on or before fixed date
  g_year <- gregorian_year_from_fixed(date)
  dates <- c(julian_in_gregorian(march, 11, g_year - 1),
             julian_in_gregorian(march, 11, g_year),
             date + 1)  # Extra to stop search
  
  n <- final(0, function(i) {
    samaritan_new_moon_after(samaritan_noon(dates[i + 1])) <= date
  })
  
  samaritan_new_moon_after(samaritan_noon(dates[n + 1]))
}

samaritan_from_fixed <- function(date) {
  # Samaritan date corresponding to fixed date
  moon <- samaritan_new_moon_at_or_before(samaritan_noon(date))
  new_year <- samaritan_new_year_on_or_before(moon)
  month <- 1 + round((moon - new_year) / 29.5)
  year <- round((new_year - samaritan_epoch) / 365.25) + 
          ceiling((month - 5) / 8)
  day <- date - moon + 1
  
  hebrew_date(year, month, day)
}

fixed_from_samaritan <- function(s_date) {
  # Fixed date of Samaritan date
  month <- standard_month(s_date)
  day <- standard_day(s_date)
  year <- standard_year(s_date)
  
  ny <- samaritan_new_year_on_or_before(
    floor(samaritan_epoch + 50 + 365.25 * (year - ceiling((month - 5) / 8))))
  nm <- samaritan_new_moon_at_or_before(ny + 29.5 * (month - 1) + 15)
  
  nm + day - 1
}

# Astronomical utility functions
solar_altitude <- function(tee, location) {
  # Geocentric altitude of sun at tee at location
  phi <- latitude(location)
  psi <- longitude(location)
  lambda <- solar_longitude(tee)
  alpha <- right_ascension(tee, 0, lambda)
  delta <- declination(tee, 0, lambda)
  theta0 <- sidereal_from_moment(tee)
  cap_H <- (theta0 - psi - alpha) %% 360
  
  altitude <- arcsin_degrees(sin_degrees(phi) * sin_degrees(delta) +
                            cos_degrees(phi) * cos_degrees(delta) * 
                            cos_degrees(cap_H))
  
  mod3(altitude, -180, 180)
}

arc_of_light <- function(tee) {
  # Angular separation of sun and moon at moment tee
  arccos_degrees(cos_degrees(lunar_latitude(tee)) * 
                 cos_degrees(lunar_phase(tee)))
}

arc_of_vision <- function(tee, location) {
  # Angular difference in altitudes of sun and moon at moment tee at location
  lunar_altitude(tee, location) - solar_altitude(tee, location)
}

lunar_semi_diameter <- function(tee, location) {
  # Topocentric lunar semi-diameter at moment tee and location
  h <- lunar_altitude(tee, location)
  p <- lunar_parallax(tee, location)
  0.27245 * p * (1 + sin_degrees(h) * sin_degrees(p))
}

shaukat_criterion <- function(date, location) {
  # S. K. Shaukat's criterion for likely visibility of crescent moon
  tee <- simple_best_view(date - 1, location)
  phase <- lunar_phase(tee)
  h <- lunar_altitude(tee, location)
  cap_ARCL <- arc_of_light(tee)
  
  (new < phase && phase < first_quarter) &&
  (deg(10.6) <= cap_ARCL && cap_ARCL <= deg(90)) &&
  (h > deg(4.1))
}

yallop_criterion <- function(date, location) {
  # B. D. Yallop's criterion for possible visibility of crescent moon
  tee <- bruin_best_view(date - 1, location)
  phase <- lunar_phase(tee)
  cap_D <- lunar_semi_diameter(tee, location)
  cap_ARCL <- arc_of_light(tee)
  cap_W <- cap_D * (1 - cos_degrees(cap_ARCL))
  cap_ARCV <- arc_of_vision(tee, location)
  e <- -0.14  # Crescent visible under perfect conditions
  q1 <- poly(cap_W, c(11.8371, -6.3226, 0.7319, -0.1018))
  
  (new < phase && phase < first_quarter) &&
  (cap_ARCV > (q1 + e))
}

simple_best_view <- function(date, location) {
  # Best viewing time (UT) in the evening - Simple version
  dark <- dusk(date, location, deg(4.5))
  best <- ifelse(dark == bogus, date + 1, dark)  # An arbitrary time
  universal_from_standard(best, location)
}

bruin_best_view <- function(date, location) {
  # Best viewing time (UT) in the evening - Yallop version, per Bruin (1977)
  sun <- sunset(date, location)
  moon <- moonset(date, location)
  
  best <- if (sun == bogus || moon == bogus) {
    date + 1  # An arbitrary time
  } else {
    (5/9) * sun + (4/9) * moon
  }
  
  universal_from_standard(best, location)
}

visible_crescent <- function(date, location) {
  # Criterion for possible visibility of crescent moon on eve of date at location
  # Shaukat's criterion may be replaced with another
  shaukat_criterion(date, location)
}
