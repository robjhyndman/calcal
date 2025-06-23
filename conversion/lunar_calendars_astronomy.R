# Section: Astronomical Lunar Calendars

babylonian_date <- function(year, month, leap, day) {
  # TYPE (babylonian-year babylonian-month
  # TYPE  babylonian-leap babylonian-day)
  # TYPE  -> babylonian-date
  list(year, month, leap, day)
}

babylonian_year <- function(date) {
  # TYPE babylonian-date -> babylonian-year
  date[[1]]
}

babylonian_month <- function(date) {
  # TYPE babylonian-date -> babylonian-month
  date[[2]]
}

babylonian_leap <- function(date) {
  # TYPE babylonian-date -> babylonian-leap
  date[[3]]
}

babylonian_day <- function(date) {
  # TYPE babylonian-date -> babylonian-day
  date[[4]]
}

moonlag <- function(date, location) {
  # TYPE (fixed-date location) -> duration
  # Time between sunset and moonset on date at location.
  # Returns bogus if there is no sunset on date.
  sun <- sunset(date, location)
  moon <- moonset(date, location)
  
  if (identical(sun, bogus)) {
    return(bogus)
  } else if (identical(moon, bogus)) {
    return(hr(24))  # Arbitrary.
  } else {
    return(moon - sun)
  }
}

# Location of Babylon.
BABYLON <- location(deg(32.4794), deg(44.4328), mt(26), hr(3 + 1/2))

# Fixed date of start of the Babylonian calendar
# (Seleucid era). April 3, 311 BCE (Julian).
BABYLONIAN_EPOCH <- fixed_from_julian(julian_date(bce(311), april, 3))

babylonian_leap_year_p <- function(b_year) {
  # TYPE babylonian-year -> boolean
  # True if b_year is a leap year on Babylonian calendar.
  (7 * b_year + 13) %% 19 < 7
}

babylonian_criterion <- function(date) {
  # TYPE (fixed-date location) -> boolean
  # Moonlag criterion for visibility of crescent moon on 
  # eve of date in Babylon.
  set <- sunset(date - 1, BABYLON)
  tee <- universal_from_standard(set, BABYLON)
  phase <- lunar_phase(tee)
  
  (new < phase && phase < first_quarter) &&
    (new_moon_before(tee) <= (tee - hr(24))) &&
    (moonlag(date - 1, BABYLON) > mn(48))
}

babylonian_new_month_on_or_before <- function(date) {
  # TYPE fixed-date -> fixed-date
  # Fixed date of start of Babylonian month on or before
  # Babylonian date. Using lag of moonset criterion.
  
  # Prior new moon.
  moon <- fixed_from_moment(lunar_phase_at_or_before(new, date))
  age <- date - moon
  
  # Check if not visible yet on eve of date.
  if (age <= 3 && !babylonian_criterion(date)) {
    tau <- moon - 30  # Must go back a month.
  } else {
    tau <- moon
  }
  
  next_func(function(d) babylonian_criterion(d), tau)
}

fixed_from_babylonian <- function(b_date) {
  # TYPE babylonian-date -> fixed-date
  # Fixed date equivalent to Babylonian date.
  month <- babylonian_month(b_date)
  leap <- babylonian_leap(b_date)
  day <- babylonian_day(b_date)
  year <- babylonian_year(b_date)
  
  # Elapsed months this year.
  if (leap || ((year %% 19) == 18 && month > 6)) {
    month1 <- month
  } else {
    month1 <- month - 1
  }
  
  # Elapsed months since epoch.
  months <- floor(((year - 1) * 235 + 13) / 19) + month1
  
  # Middle of given month.
  midmonth <- BABYLONIAN_EPOCH + round(MEAN_SYNODIC_MONTH * months) + 15
  
  babylonian_new_month_on_or_before(midmonth) + day - 1
}

babylonian_from_fixed <- function(date) {
  # TYPE fixed-date -> babylonian-date
  # Babylonian date corresponding to fixed date.
  
  # Most recent new month.
  crescent <- babylonian_new_month_on_or_before(date)
  
  # Elapsed months since epoch.
  months <- round((crescent - BABYLONIAN_EPOCH) / MEAN_SYNODIC_MONTH)
  
  year <- 1 + floor((19 * months + 5) / 235)
  
  # Approximate date of new year.
  approx <- BABYLONIAN_EPOCH + 
    round(floor(((year - 1) * 235 + 13) / 19) * MEAN_SYNODIC_MONTH)
  
  new_year <- babylonian_new_month_on_or_before(approx + 15)
  month1 <- 1 + round((crescent - new_year) / 29.5)
  special <- (year %% 19) == 18
  leap <- if (special) (month1 == 7) else (month1 == 13)
  
  if (leap || (special && month1 > 6)) {
    month <- month1 - 1
  } else {
    month <- month1
  }
  
  day <- date - crescent + 1
  
  babylonian_date(year, month, leap, day)
}

phasis_on_or_before <- function(date, location) {
  # TYPE (fixed-date location) -> fixed-date
  # Closest fixed date on or before date when crescent
  # moon first became visible at location.
  
  # Prior new moon.
  moon <- fixed_from_moment(lunar_phase_at_or_before(new, date))
  age <- date - moon
  
  # Check if not visible yet on eve of date.
  if (age <= 3 && !visible_crescent(date, location)) {
    tau <- moon - 30  # Must go back a month.
  } else {
    tau <- moon
  }
  
  next_func(function(d) visible_crescent(d, location), tau)
}

phasis_on_or_after <- function(date, location) {
  # TYPE (fixed-date location) -> fixed-date
  # Closest fixed date on or after date on the eve
  # of which crescent moon first became visible at location.
  
  # Prior new moon.
  moon <- fixed_from_moment(lunar_phase_at_or_before(new, date))
  age <- date - moon
  
  # Check if not visible yet on eve of date.
  if (age >= 4 || visible_crescent(date - 1, location)) {
    tau <- moon + 29  # Next new moon
  } else {
    tau <- date
  }
  
  next_func(function(d) visible_crescent(d, location), tau)
}

# Sample location for Observational Islamic calendar
# (Cairo, Egypt).
ISLAMIC_LOCATION <- location(deg(30.1), deg(31.3), mt(200), hr(2))

fixed_from_observational_islamic <- function(i_date) {
  # TYPE islamic-date -> fixed-date
  # Fixed date equivalent to Observational Islamic date.
  month <- standard_month(i_date)
  day <- standard_day(i_date)
  year <- standard_year(i_date)
  
  # Middle of given month.
  midmonth <- ISLAMIC_EPOCH + 
    floor(((year - 1) * 12 + month - 1/2) * MEAN_SYNODIC_MONTH)
  
  # First day of month.
  phasis_on_or_before(midmonth, ISLAMIC_LOCATION) + day - 1
}

observational_islamic_from_fixed <- function(date) {
  # TYPE fixed-date -> islamic-date
  # Observational Islamic date (year month day)
  # corresponding to fixed date.
  
  # Most recent new moon.
  crescent <- phasis_on_or_before(date, ISLAMIC_LOCATION)
  
  elapsed_months <- round((crescent - ISLAMIC_EPOCH) / MEAN_SYNODIC_MONTH)
  year <- 1 + floor(elapsed_months / 12)
  month <- 1 + (elapsed_months %% 12)
  day <- 1 + (date - crescent)
  
  islamic_date(year, month, day)
}

# Location of Jerusalem.
JERUSALEM <- location(deg(31.78), deg(35.24), mt(740), hr(2))

# Location of Acre.
ACRE <- location(deg(32.94), deg(35.09), mt(22), hr(2))

astronomical_easter <- function(g_year) {
  # TYPE gregorian-year -> fixed-date
  # Date of (proposed) astronomical Easter in Gregorian year.
  
  # Spring equinox.
  equinox <- season_in_gregorian(spring, g_year)
  
  # Date of next full moon.
  paschal_moon <- floor(apparent_from_universal(
    lunar_phase_at_or_after(full, equinox), JERUSALEM))
  
  # Return the Sunday following the Paschal moon.
  kday_after(sunday, paschal_moon)
}

saudi_criterion <- function(date) {
  # TYPE fixed-date -> boolean
  # Saudi visibility criterion on eve of fixed date in Mecca.
  set <- sunset(date - 1, mecca)
  tee <- universal_from_standard(set, mecca)
  phase <- lunar_phase(tee)
  
  (new < phase && phase < first_quarter) &&
    (moonlag(date - 1, mecca) > 0)
}

saudi_new_month_on_or_before <- function(date) {
  # TYPE fixed-date -> fixed-date
  # Closest fixed date on or before date when Saudi
  # visibility criterion held.
  
  # Prior new moon.
  moon <- fixed_from_moment(lunar_phase_at_or_before(new, date))
  age <- date - moon
  
  # Check if not visible yet on eve of date.
  if (age <= 3 && !saudi_criterion(date)) {
    tau <- moon - 30  # Must go back a month.
  } else {
    tau <- moon
  }
  
  next_func(function(d) saudi_criterion(d), tau)
}

fixed_from_saudi_islamic <- function(s_date) {
  # TYPE islamic-date -> fixed-date
  # Fixed date equivalent to Saudi Islamic date.
  month <- standard_month(s_date)
  day <- standard_day(s_date)
  year <- standard_year(s_date)
  
  # Middle of given month.
  midmonth <- ISLAMIC_EPOCH + 
    floor(((year - 1) * 12 + month - 1/2) * MEAN_SYNODIC_MONTH)
  
  # First day of month.
  saudi_new_month_on_or_before(midmonth) + day - 1
}

saudi_islamic_from_fixed <- function(date) {
  # TYPE fixed-date -> islamic-date
  # Saudi Islamic date (year month day) corresponding to fixed date.
  
  # Most recent new month.
  crescent <- saudi_new_month_on_or_before(date)
  
  elapsed_months <- round((crescent - ISLAMIC_EPOCH) / MEAN_SYNODIC_MONTH)
  year <- 1 + floor(elapsed_months / 12)
  month <- 1 + (elapsed_months %% 12)
  day <- 1 + (date - crescent)
  
  islamic_date(year, month, day)
}

# Sample location for Observational Hebrew calendar
# (Haifa, Israel).
HEBREW_LOCATION <- location(deg(32.82), deg(35), mt(0), hr(2))

observational_hebrew_first_of_nisan <- function(g_year) {
  # TYPE gregorian-year -> fixed-date
  # Fixed date of Observational (classical)
  # Nisan 1 occurring in Gregorian year.
  
  # Spring equinox.
  equinox <- season_in_gregorian(spring, g_year)
  
  # Moment (UT) of sunset on day of equinox.
  set <- universal_from_standard(
    sunset(floor(equinox), HEBREW_LOCATION), HEBREW_LOCATION)
  
  phasis_on_or_after(
    floor(equinox) - if (equinox < set) 14 else 13,
    HEBREW_LOCATION)
}

fixed_from_observational_hebrew <- function(h_date) {
  # TYPE hebrew-date -> fixed-date
  # Fixed date equivalent to Observational Hebrew date.
  month <- standard_month(h_date)
  day <- standard_day(h_date)
  year <- standard_year(h_date)
  
  year1 <- if (month >= tishri) (year - 1) else year
  start <- fixed_from_hebrew(hebrew_date(year1, nisan, 1))
  g_year <- gregorian_year_from_fixed(start + 60)
  new_year <- observational_hebrew_first_of_nisan(g_year)
  
  # Middle of given month.
  midmonth <- new_year + round(29.5 * (month - 1)) + 15
  
  # First day of month.
  phasis_on_or_before(midmonth, HEBREW_LOCATION) + day - 1
}

observational_hebrew_from_fixed <- function(date) {
  # TYPE fixed-date -> hebrew-date
  # Observational Hebrew date (year month day)
  # corresponding to fixed date.
  
  # Most recent new moon.
  crescent <- phasis_on_or_before(date, HEBREW_LOCATION)
  
  g_year <- gregorian_year_from_fixed(date)
  ny <- observational_hebrew_first_of_nisan(g_year)
  
  if (date < ny) {
    new_year <- observational_hebrew_first_of_nisan(g_year - 1)
  } else {
    new_year <- ny
  }
  
  month <- 1 + round((crescent - new_year) / 29.5)
  year <- standard_year(hebrew_from_fixed(new_year)) + 
    if (month >= tishri) 1 else 0
  day <- date - crescent + 1
  
  hebrew_date(year, month, day)
}

month_length <- function(date, location) {
  # TYPE (fixed-date location) -> 1..31
  # Length of lunar month based on observability at location,
  # which includes date.
  moon <- phasis_on_or_after(date + 1, location)
  prev <- phasis_on_or_before(date, location)
  moon - prev
}

early_month_p <- function(date, location) {
  # TYPE (fixed-date location) -> boolean
  # Fixed date in location is in a month that was forced to
  # start early.
  start <- phasis_on_or_before(date, location)
  prev <- start - 15
  
  ((date - start) >= 30) ||
    (month_length(prev, location) > 30) ||
    ((month_length(prev, location) == 30) && early_month_p(prev, location))
}

alt_fixed_from_observational_islamic <- function(i_date) {
  # TYPE islamic-date -> fixed-date
  # Fixed date equivalent to Observational Islamic date.
  # Months are never longer than 30 days.
  month <- standard_month(i_date)
  day <- standard_day(i_date)
  year <- standard_year(i_date)
  
  # Middle of given month.
  midmonth <- ISLAMIC_EPOCH + 
    floor(((year - 1) * 12 + month - 1/2) * MEAN_SYNODIC_MONTH)
  
  # First day of month.
  moon <- phasis_on_or_before(midmonth, ISLAMIC_LOCATION)
  date <- moon + day - 1
  
  if (early_month_p(midmonth, ISLAMIC_LOCATION)) (date - 1) else date
}

alt_observational_islamic_from_fixed <- function(date) {
  # TYPE fixed-date -> islamic-date
  # Observational Islamic date (year month day)
  # corresponding to fixed date.
  # Months are never longer than 30 days.
  early <- early_month_p(date, ISLAMIC_LOCATION)
  long <- early && (month_length(date, ISLAMIC_LOCATION) > 29)
  
  date_prime <- if (long) (date + 1) else date
  
  # Most recent new moon.
  moon <- phasis_on_or_before(date_prime, ISLAMIC_LOCATION)
  
  elapsed_months <- round((moon - ISLAMIC_EPOCH) / MEAN_SYNODIC_MONTH)
  year <- 1 + floor(elapsed_months / 12)
  month <- 1 + (elapsed_months %% 12)
  day <- date_prime - moon + if (early && !long) 2 else 1
  
  islamic_date(year, month, day)
}

alt_observational_hebrew_from_fixed <- function(date) {
  # TYPE fixed-date -> hebrew-date
  # Observational Hebrew date (year month day)
  # corresponding to fixed date.
  # Months are never longer than 30 days.
  early <- early_month_p(date, HEBREW_LOCATION)
  long <- early && (month_length(date, HEBREW_LOCATION) > 29)
  
  date_prime <- if (long) (date + 1) else date
  
  # Most recent new moon.
  moon <- phasis_on_or_before(date_prime, HEBREW_LOCATION)
  
  g_year <- gregorian_year_from_fixed(date_prime)
  ny <- observational_hebrew_first_of_nisan(g_year)
  
  if (date_prime < ny) {
    new_year <- observational_hebrew_first_of_nisan(g_year - 1)
  } else {
    new_year <- ny
  }
  
  month <- 1 + round((moon - new_year) / 29.5)
  year <- standard_year(hebrew_from_fixed(new_year)) + 
    if (month >= tishri) 1 else 0
  day <- date_prime - moon + if (early && !long) 2 else 1
  
  hebrew_date(year, month, day)
}

alt_fixed_from_observational_hebrew <- function(h_date) {
  # TYPE hebrew-date -> fixed-date
  # Fixed date equivalent to Observational Hebrew date.
  # Months are never longer than 30 days.
  month <- standard_month(h_date)
  day <- standard_day(h_date)
  year <- standard_year(h_date)
  
  year1 <- if (month >= tishri) (year - 1) else year
  start <- fixed_from_hebrew(hebrew_date(year1, nisan, 1))
  g_year <- gregorian_year_from_fixed(start + 60)
  new_year <- observational_hebrew_first_of_nisan(g_year)
  
  # Middle of given month.
  midmonth <- new_year + round(29.5 * (month - 1)) + 15
  
  # First day of month.
  moon <- phasis_on_or_before(midmonth, HEBREW_LOCATION)
  date <- moon + day - 1
  
  if (early_month_p(midmonth, HEBREW_LOCATION)) (date - 1) else date
}

classical_passover_eve <- function(g_year) {
  # TYPE gregorian-year -> fixed-date
  # Fixed date of Classical (observational) Passover Eve
  # (Nisan 14) occurring in Gregorian year.
  observational_hebrew_first_of_nisan(g_year) + 13
}

# Location of Mt. Gerizim.
SAMARITAN_LOCATION <- location(deg(32.1994), deg(35.2728), mt(881), hr(2))

# Fixed date of start of the Samaritan Entry Era.
SAMARITAN_EPOCH <- fixed_from_julian(julian_date(bce(1639), march, 15))

samaritan_noon <- function(date) {
  # TYPE fixed-date -> moment
  # Universal time of true noon on date at Samaritan location.
  midday(date, SAMARITAN_LOCATION)
}

samaritan_new_moon_after <- function(tee) {
  # TYPE moment -> fixed-date
  # Fixed date of first new moon after UT moment tee.
  # Modern calculation.
  ceiling(apparent_from_universal(new_moon_at_or_after(tee), 
                                  SAMARITAN_LOCATION) - hr(12))
}

samaritan_new_moon_at_or_before <- function(tee) {
  # TYPE moment -> fixed-date
  # Fixed-date of last new moon before UT moment tee.
  # Modern calculation.
  ceiling(apparent_from_universal(new_moon_before(tee), 
                                  SAMARITAN_LOCATION) - hr(12))
}

samaritan_new_year_on_or_before <- function(date) {
  # TYPE fixed-date -> fixed-date
  # Fixed date of Samaritan New Year on or before fixed date.
  g_year <- gregorian_year_from_fixed(date)
  
  # All possible March 11's.
  dates <- c(julian_in_gregorian(march, 11, g_year - 1),
             julian_in_gregorian(march, 11, g_year),
             date + 1)  # Extra to stop search.
  
  n <- final_func(function(i) {
    samaritan_new_moon_after(samaritan_noon(dates[i])) <= date
  }, 1)
  
  samaritan_new_moon_after(samaritan_noon(dates[n]))
}

samaritan_from_fixed <- function(date) {
  # TYPE fixed-date -> hebrew-date
  # Samaritan date corresponding to fixed date.
  
  # First of month
  moon <- samaritan_new_moon_at_or_before(samaritan_noon(date))
  new_year <- samaritan_new_year_on_or_before(moon)
  month <- 1 + round((moon - new_year) / 29.5)
  year <- round((new_year - SAMARITAN_EPOCH) / 365.25) + 
    ceiling((month - 5) / 8)
  day <- date - moon + 1
  
  hebrew_date(year, month, day)
}

fixed_from_samaritan <- function(s_date) {
  # TYPE hebrew-date -> fixed-date
  # Fixed date of Samaritan date.
  month <- standard_month(s_date)
  day <- standard_day(s_date)
  year <- standard_year(s_date)
  
  ny <- samaritan_new_year_on_or_before(
    floor(SAMARITAN_EPOCH + 50 + 365.25 * (year - ceiling((month - 5) / 8))))
  
  nm <- samaritan_new_moon_at_or_before(ny + 29.5 * (month - 1) + 15)
  
  nm + day - 1
}

solar_altitude <- function(tee, location) {
  # TYPE (moment location) -> half-circle
  # Geocentric altitude of sun at tee at location,
  # as a positive/negative angle in degrees, ignoring
  # parallax and refraction.
  
  # Local latitude.
  phi <- latitude(location)
  # Local longitude.
  psi <- longitude(location)
  # Solar longitude.
  lambda <- solar_longitude(tee)
  # Solar right ascension.
  alpha <- right_ascension(tee, 0, lambda)
  # Solar declination.
  delta <- declination(tee, 0, lambda)
  # Sidereal time.
  theta0 <- sidereal_from_moment(tee)
  # Local hour angle.
  cap_H <- (theta0 - psi - alpha) %% 360
  
  altitude <- arcsin_degrees(sin_degrees(phi) * sin_degrees(delta) +
                             cos_degrees(phi) * cos_degrees(delta) * 
                             cos_degrees(cap_H))
  
  mod3(altitude, -180, 180)
}

arc_of_light <- function(tee) {
  # TYPE moment -> half-circle
  # Angular separation of sun and moon at moment tee.
  arccos_degrees(cos_degrees(lunar_latitude(tee)) * 
                 cos_degrees(lunar_phase(tee)))
}

arc_of_vision <- function(tee, location) {
  # TYPE (moment location) -> half-circle
  # Angular difference in altitudes of sun and moon
  # at moment tee at location.
  lunar_altitude(tee, location) - solar_altitude(tee, location)
}

lunar_semi_diameter <- function(tee, location) {
  # TYPE (moment location) -> half-circle
  # Topocentric lunar semi-diameter at moment tee and location.
  h <- lunar_altitude(tee, location)
  p <- lunar_parallax(tee, location)
  0.27245 * p * (1 + sin_degrees(h) * sin_degrees(p))
}

shaukat_criterion <- function(date, location) {
  # TYPE (fixed-date location) -> boolean
  # S. K. Shaukat's criterion for likely
  # visibility of crescent moon on eve of date at location.
  # Not intended for high altitudes or polar regions.
  tee <- simple_best_view(date - 1, location)
  phase <- lunar_phase(tee)
  h <- lunar_altitude(tee, location)
  cap_ARCL <- arc_of_light(tee)
  
  (new < phase && phase < first_quarter) &&
    (deg(10.6) <= cap_ARCL && cap_ARCL <= deg(90)) &&
    (h > deg(4.1))
}

yallop_criterion <- function(date, location) {
  # TYPE (fixed-date location) -> boolean
  # B. D. Yallop's criterion for possible
  # visibility of crescent moon on eve of date at location.
  # Not intended for high altitudes or polar regions.
  
  # Best viewing time prior evening.
  tee <- bruin_best_view(date - 1, location)
  phase <- lunar_phase(tee)
  cap_D <- lunar_semi_diameter(tee, location)
  cap_ARCL <- arc_of_light(tee)
  cap_W <- cap_D * (1 - cos_degrees(cap_ARCL))
  cap_ARCV <- arc_of_vision(tee, location)
  e <- -0.14  # Crescent visible under perfect conditions.
  q1 <- poly(cap_W, c(11.8371, -6.3226, 0.7319, -0.1018))
  
  (new < phase && phase < first_quarter) &&
    (cap_ARCV > (q1 + e))
}

simple_best_view <- function(date, location) {
  # TYPE (fixed-date location) -> moment
  # Best viewing time (UT) in the evening.
  # Simple version.
  
  # Best viewing time prior evening.
  dark <- dusk(date, location, deg(4.5))
  
  if (identical(dark, bogus)) {
    best <- date + 1  # An arbitrary time.
  } else {
    best <- dark
  }
  
  universal_from_standard(best, location)
}

bruin_best_view <- function(date, location) {
  # TYPE (fixed-date location) -> moment
  # Best viewing time (UT) in the evening.
  # Yallop version, per Bruin (1977).
  sun <- sunset(date, location)
  moon <- moonset(date, location)
  
  # Best viewing time prior evening.
  if (identical(sun, bogus) || identical(moon, bogus)) {
    best <- date + 1  # An arbitrary time.
  } else {
    best <- (5/9) * sun + (4/9) * moon
  }
  
  universal_from_standard(best, location)
}

visible_crescent <- function(date, location) {
  # TYPE (fixed-date location) -> boolean
  # Criterion for possible visibility of crescent moon
  # on eve of date at location.
  # Shaukat's criterion may be replaced with another.
  shaukat_criterion(date, location)
}
