# Section: Astronomical Lunar Calendars

# Sample location for Observational Islamic calendar
# (Cairo, Egypt).
ISLAMIC_LOCATION <- location(deg(30.1), deg(31.3), mt(200), 2)
MECCA <- location(angle(21, 25, 24), angle(39, 49, 24), mt(298), 3)
# Location of Jerusalem.
JERUSALEM <- location(deg(31.78), deg(35.24), mt(740), 2)
# Location of Acre.
ACRE <- location(deg(32.94), deg(35.09), mt(22), 2)
# Sample location for Observational Hebrew calendar
# (Haifa, Israel).
HEBREW_LOCATION <- location(deg(32.82), deg(35), mt(0), 2)
# Location of Mt. Gerizim.
SAMARITAN_LOCATION <- location(deg(32.1994), deg(35.2728), mt(881), 2)
# Fixed date of start of the Samaritan Entry Era.
SAMARITAN_EPOCH <- -598573 # vec_data(julian_date(bce(1639), MARCH, 15))


fixed_from_observational_islamic <- function(i_date) {
  # TYPE islamic-date -> fixed-date
  # Fixed date equivalent to Observational Islamic date.
  # Middle of given month.
  midmonth <- ISLAMIC_EPOCH +
    floor(((i_date$year - 1) * 12 + i_date$month - 1 / 2) * MEAN_SYNODIC_MONTH)

  # First day of month.
  phasis_on_or_before(midmonth, ISLAMIC_LOCATION) + i_date$day - 1
}

observational_islamic_from_fixed <- function(date) {
  # TYPE fixed-date -> islamic-date
  # Observational Islamic date (year month day)
  # corresponding to fixed date.
  date <- vec_data(date)

  # Most recent new moon.
  crescent <- phasis_on_or_before(date, ISLAMIC_LOCATION)

  elapsed_months <- round((crescent - ISLAMIC_EPOCH) / MEAN_SYNODIC_MONTH)
  year <- 1 + floor(elapsed_months / 12)
  month <- 1 + (elapsed_months %% 12)
  day <- 1 + (date - crescent)

  list(year = year, month = month, day = day)
}

validate_oslamic <- function(date) {}


fixed_from_saudi_islamic <- function(s_date) {
  # TYPE islamic-date -> fixed-date
  # Fixed date equivalent to Saudi Islamic date.
  # Middle of given month.
  midmonth <- ISLAMIC_EPOCH +
    floor(((s_date$year - 1) * 12 + s_date$month - 1 / 2) * MEAN_SYNODIC_MONTH)

  # First day of month.
  saudi_new_month_on_or_before(midmonth) + s_date$day - 1
}

saudi_islamic_from_fixed <- function(date) {
  # TYPE fixed-date -> islamic-date
  # Saudi Islamic date (year month day) corresponding to fixed date.
  date <- vec_data(date)
  # Most recent new month.
  crescent <- saudi_new_month_on_or_before(date)

  elapsed_months <- round((crescent - ISLAMIC_EPOCH) / MEAN_SYNODIC_MONTH)
  year <- 1 + floor(elapsed_months / 12)
  month <- 1 + (elapsed_months %% 12)
  day <- 1 + (date - crescent)

  list(year = year, month = month, day = day)
}

fixed_from_observational_hebrew <- function(h_date) {
  # TYPE hebrew-date -> fixed-date
  # Fixed date equivalent to Observational Hebrew date.
  year1 <- h_date$year - (h_date$month >= TISHRI)
  start <- fixed_from_hebrew(hebrew_date(year1, NISAN, 1))
  g_year <- gregorian_year_from_fixed(start + 60)
  new_year <- observational_hebrew_first_of_nisan(g_year)

  # Middle of given month.
  midmonth <- new_year + round(29.5 * (h_date$month - 1)) + 15

  # First day of month.
  phasis_on_or_before(midmonth, HEBREW_LOCATION) + h_date$day - 1
}

observational_hebrew_from_fixed <- function(date) {
  # TYPE fixed-date -> hebrew-date
  # Observational Hebrew date (year month day)
  # corresponding to fixed date.
  date <- vec_data(date)
  # Most recent new moon.
  crescent <- phasis_on_or_before(date, HEBREW_LOCATION)

  g_year <- gregorian_year_from_fixed(date)
  ny <- observational_hebrew_first_of_nisan(g_year)

  new_year <- ny
  if (any(date < ny, na.rm = TRUE)) {
    new_year[date < ny] <- observational_hebrew_first_of_nisan(
      g_year[date < ny] - 1
    )
  }

  month <- 1 + round((crescent - new_year) / 29.5)
  year <- hebrew_from_fixed(new_year)$year +
    (month >= TISHRI)
  day <- date - crescent + 1

  list(year = year, month = month, day = day)
}


samaritan_from_fixed <- function(date) {
  # TYPE fixed-date -> hebrew-date
  # Samaritan date corresponding to fixed date.
  date <- vec_data(date)
  # First of month
  moon <- nth_new_moon(
    samaritan_new_moon_at_or_before(
      samaritan_noon(date)
    )
  )
  new_year <- samaritan_new_year_on_or_before(moon)
  month <- 1 + round((moon - new_year) / 29.5)
  year <- round((new_year - SAMARITAN_EPOCH) / 365.25) +
    ceiling((month - 5) / 8)
  day <- trunc(date - moon + 1)

  list(year = year, month = month, day = day)
}

fixed_from_samaritan <- function(s_date) {
  # TYPE hebrew-date -> fixed-date
  # Fixed date of Samaritan date.
  ny <- samaritan_new_year_on_or_before(
    floor(
      SAMARITAN_EPOCH +
        50 +
        365.25 * (s_date$year - ceiling((s_date$month - 5) / 8))
    )
  )
  nm <- samaritan_new_moon_at_or_before(ny + 29.5 * (s_date$month - 1) + 15)
  nm + s_date$day - 1
}


#' @rdname cal_calendar
#' @format NULL
#' @export
cal_oislamic <- cal_calendar(
  "oislamic",
  "OHij",
  c("year","month","day"),
  validate_oslamic,
  format_islamic,
  observational_islamic_from_fixed,
  fixed_from_observational_islamic
)

#' @rdname cal_calendar
#' @format NULL
#' @export
cal_saudi <- cal_calendar(
  "saudi",
  "SHij",
  c("year","month","day"),
  validate_oslamic,
  format_islamic,
  saudi_islamic_from_fixed,
  fixed_from_saudi_islamic
)

#' @rdname cal_calendar
#' @format NULL
#' @export
cal_ohebrew <- cal_calendar(
  "ohebrew",
  "OHeb",
  c("year","month","day"),
  validate_hebrew,
  format_hebrew,
  observational_hebrew_from_fixed,
  fixed_from_observational_hebrew
)

#' @rdname cal_calendar
#' @format NULL
#' @export
cal_samaritan <- cal_calendar(
  "samaritan",
  "Sam",
  c("year","month","day"),
  validate_hebrew,
  format_hebrew,
  samaritan_from_fixed,
  fixed_from_samaritan
)

#' @rdname islamic
oislamic_date <- function(year, month, day) {
  new_date(year = year, month = month, day = day, calendar = cal_oislamic)
}

#' @rdname islamic
as_oislamic <- function(date) {
  as_date(date, calendar = cal_oislamic)
}

#' @rdname islamic
saudi_date <- function(year, month, day) {
  new_date(year = year, month = month, day = day, calendar = cal_saudi)
}

#' @rdname islamic
as_saudi <- function(date) {
  as_date(date, calendar = cal_saudi)
}

#' @rdname hebrew
ohebrew_date <- function(year = integer(), month = integer(), day = integer()) {
  new_date(year = year, month = month, day = day, calendar = cal_ohebrew)
}

#' @rdname hebrew
as_ohebrew <- function(date) {
  as_date(date, calendar = cal_ohebrew)
}

#' @rdname hebrew
samaritan_date <- function(
  year = integer(),
  month = integer(),
  day = integer()
) {
  new_date(year = year, month = month, day = day, calendar = cal_samaritan)
}

#' @rdname hebrew
as_samaritan <- function(date) {
  as_date(date, calendar = cal_samaritan)
}

#' @rdname christian
#' @export
astronomical_easter <- function(year) {
  # Date of (proposed) astronomical Easter in Gregorian year.

  # Spring equinox.
  equinox <- season_in_gregorian(SPRING, year)

  # Date of next full moon.
  paschal_moon <- floor(apparent_from_universal(
    lunar_phase_at_or_after(FULL, equinox),
    JERUSALEM
  ))

  # Return the Sunday following the Paschal moon.
  as_gregorian(kday_after(SUNDAY, paschal_moon))
}

saudi_criterion <- function(date) {
  # TYPE fixed-date -> boolean
  # Saudi visibility criterion on eve of fixed date in Mecca.
  set <- as.numeric(date) + as.numeric(sunset(date - 1, MECCA))
  tee <- universal_from_standard(set, MECCA)
  phase <- lunar_phase(tee)

  (NEW < phase & phase < FIRST_QUARTER) &
    (moonlag(date - 1, MECCA) > 0)
}

saudi_new_month_on_or_before <- function(date) {
  # TYPE fixed-date -> fixed-date
  # Closest fixed date on or before date when Saudi
  # visibility criterion held.

  # Prior new moon.
  moon <- fixed_from_moment(lunar_phase_at_or_before(NEW, date))
  age <- date - moon
  tau <- moon - 30 * (age <= 3 & !saudi_criterion(date))
  next_value(tau, saudi_criterion)
}


observational_hebrew_first_of_nisan <- function(g_year) {
  # TYPE gregorian-year -> fixed-date
  # Fixed date of Observational (classical)
  # Nisan 1 occurring in Gregorian year.

  # Spring equinox.
  equinox <- season_in_gregorian(rep(SPRING, length(g_year)), g_year)

  # Moment (UT) of sunset on day of equinox.
  set <- universal_from_standard(
    as.numeric(sunset(floor(equinox), HEBREW_LOCATION)),
    rep(HEBREW_LOCATION, length(g_year))
  ) +
    floor(equinox)

  phasis_on_or_after(
    floor(equinox) - if (equinox < set) 14 else 13,
    HEBREW_LOCATION
  )
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

  ((date - start) >= 30) |
    (month_length(prev, location) > 30) |
    ((month_length(prev, location) == 30) & early_month_p(prev, location))
}


classical_passover_eve <- function(g_year) {
  # TYPE gregorian-year -> fixed-date
  # Fixed date of Classical (observational) Passover Eve
  # (Nisan 14) occurring in Gregorian year.
  observational_hebrew_first_of_nisan(g_year) + 13
}

samaritan_noon <- function(date) {
  # TYPE fixed-date -> moment
  # Universal time of true noon on date at Samaritan location.
  midday(date, SAMARITAN_LOCATION)
}

samaritan_new_moon_after <- function(tee) {
  # TYPE moment -> fixed-date
  # Fixed date of first new moon after UT moment tee.
  # Modern calculation.
  ceiling(
    apparent_from_universal(new_moon_at_or_after(tee), SAMARITAN_LOCATION) -
      hr(12)
  )
}

samaritan_new_moon_at_or_before <- function(tee) {
  # TYPE moment -> fixed-date
  # Fixed-date of last new moon before UT moment tee.
  # Modern calculation.
  ceiling(
    apparent_from_universal(new_moon_before(tee), SAMARITAN_LOCATION) - hr(12)
  )
}

samaritan_new_year_on_or_before <- function(date) {
  # TYPE fixed-date -> fixed-date
  # Fixed date of Samaritan New Year on or before fixed date.
  g_year <- gregorian_year_from_fixed(date)

  one_date <- function(date, g_year) {
    # All possible March 11's.
    dates <- julian_in_gregorian(MARCH, 11, g_year - c(1, 0))
    nnm <- nth_new_moon(samaritan_new_moon_after(samaritan_noon(dates)))
    utils::tail(nnm[nnm <= date], 1)
  }
  unlist(mapply(one_date, date, g_year))
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

  altitude <- arcsin_degrees(
    sin_degrees(phi) *
      sin_degrees(delta) +
      cos_degrees(phi) * cos_degrees(delta) * cos_degrees(cap_H)
  )

  mod3(altitude, -180, 180)
}

arc_of_light <- function(tee) {
  # TYPE moment -> half-circle
  # Angular separation of sun and moon at moment tee.
  arccos_degrees(
    cos_degrees(lunar_latitude(tee)) *
      cos_degrees(lunar_phase(tee))
  )
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

  (NEW < phase & phase < FIRST_QUARTER) &
    (deg(10.6) <= cap_ARCL & cap_ARCL <= deg(90)) &
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
  e <- -0.14 # Crescent visible under perfect conditions.
  q1 <- poly(cap_W, c(11.8371, -6.3226, 0.7319, -0.1018))

  (NEW < phase & phase < FIRST_QUARTER) &
    (cap_ARCV > (q1 + e))
}

simple_best_view <- function(date, location) {
  # TYPE (fixed-date location) -> moment
  # Best viewing time (UT) in the evening.
  # Simple version.

  # Best viewing time prior evening.
  dark <- dusk(date, location, deg(4.5))
  best <- dark
  best[is.na(best)] <- date[is.na(best)] + 1 # An arbitrary time

  universal_from_standard(best, location)
}

bruin_best_view <- function(date, location) {
  # TYPE (fixed-date location) -> moment
  # Best viewing time (UT) in the evening.
  # Yallop version, per Bruin (1977).
  sun <- as.numeric(date) + as.numeric(sunset(date, location))
  moon <- as.numeric(date) + as.numeric(moonset(date, location))
  # Best viewing time prior evening.
  best <- (5 / 9) * sun + (4 / 9) * moon
  best[is.na(best)] <- date[is.na(best)] + 1 # An arbitrary time

  universal_from_standard(best, location)
}

visible_crescent <- function(date, location) {
  # TYPE (fixed-date location) -> boolean
  # Criterion for possible visibility of crescent moon
  # on eve of date at location.
  # Shaukat's criterion may be replaced with another.
  shaukat_criterion(date, location)
}


phasis_on_or_before <- function(date, location) {
  # TYPE (fixed-date location) -> fixed-date
  # Closest fixed date on or before date when crescent
  # moon first became visible at location.
  lst <- vctrs::vec_recycle_common(date = date, location = location)
  # Prior new moon.
  moon <- fixed_from_moment(lunar_phase_at_or_before(
    rep(NEW, length(date)),
    lst$date
  ))
  age <- lst$date - moon
  tau <- moon - 30 * (age <= 3 & !visible_crescent(lst$date, lst$location))
  next_value(tau, function(x) {
    visible_crescent(x, lst$location)
  })
}

phasis_on_or_after <- function(date, location) {
  # TYPE (fixed-date location) -> fixed-date
  # Closest fixed date on or after date on the eve
  # of which crescent moon first became visible at location.
  lst <- vctrs::vec_recycle_common(date = date, location = location)

  # Prior new moon.
  moon <- fixed_from_moment(lunar_phase_at_or_before(
    rep(NEW, length(date)),
    lst$date
  ))
  age <- lst$date - moon
  tau <- lst$date
  # Check if not visible yet on eve of date.
  not_visible <- age >= 4 | visible_crescent(lst$date - 1, lst$location)
  if (any(not_visible, na.rm = TRUE)) {
    tau[not_visible] <- moon[not_visible] + 29 # Next new moon
  }
  next_value(tau, function(x) {
    visible_crescent(x, lst$location)
  })
}
