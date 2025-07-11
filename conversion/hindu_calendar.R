#### Section: Modern Hindu Calendars

hindu_lunar_date <- function(year, month, leap_month, day, leap_day) {
  # TYPE (hindu-lunar-year hindu-lunar-month
  # TYPE  hindu-lunar-leap-month hindu-lunar-day
  # TYPE  hindu-lunar-leap-day) -> hindu-lunar-date
  list(year, month, leap_month, day, leap_day)
}

hindu_lunar_month <- function(date) {
  # TYPE hindu-lunar_date -> hindu-lunar-month
  date[[2]]
}

hindu_lunar_leap_month <- function(date) {
  # TYPE hindu_lunar_date -> hindu_lunar_leap_month
  date[[3]]
}

hindu_lunar_day <- function(date) {
  # TYPE hindu_lunar_date -> hindu_lunar_day
  date[[4]]
}

hindu_lunar_leap_day <- function(date) {
  # TYPE hindu_lunar_date -> hindu_lunar_leap_day
  date[[5]]
}

hindu_lunar_year <- function(date) {
  # TYPE hindu_lunar_date -> hindu_lunar_year
  date[[1]]
}

hindu_sine_table <- function(entry) {
  # TYPE integer -> rational-amplitude
  # This simulates the Hindu sine table.
  # $entry$ is an angle given as a multiplier of 225'.
  exact <- 3438 * sin_degrees(entry * angle(0, 225, 0))
  error <- 0.215 * sign(exact) * sign(abs(exact) - 1716)
  round(exact + error) / 3438
}

hindu_sine <- function(theta) {
  # TYPE rational-angle -> rational-amplitude
  # Linear interpolation for $theta$ in Hindu table.
  entry <- theta / angle(0, 225, 0) # Interpolate in table.
  fraction <- entry %% 1
  fraction *
    hindu_sine_table(ceiling(entry)) +
    (1 - fraction) * hindu_sine_table(floor(entry))
}

hindu_arcsin <- function(amp) {
  # TYPE rational-amplitude -> rational-angle
  # Inverse of Hindu sine function of $amp$.
  if (amp < 0) {
    -hindu_arcsin(-amp)
  } else {
    pos <- next(k, 0, amp <= hindu_sine_table(k))
    below <- hindu_sine_table(pos - 1) # Lower value in table.
    angle(0, 225, 0) *
      (pos -
        1 + # Interpolate.
        (amp - below) / (hindu_sine_table(pos) - below))
  }
}

hindu_sidereal_year <- 365 + 279457/1080000
# TYPE rational
# Mean length of Hindu sidereal year.

hindu_creation <- hindu_epoch - 1955880000 * hindu_sidereal_year
# TYPE fixed-date
# Fixed date of Hindu creation.

hindu_mean_position <- function(tee, period) {
  # TYPE (rational-moment rational) -> rational-angle
  # Position in degrees at moment $tee$ in uniform circular
  # orbit of $period$ days.
  deg(360) * ((tee - hindu_creation) / period %% 1)
}

hindu_sidereal_month <- 27 + 4644439/14438334
# TYPE rational
# Mean length of Hindu sidereal month.

hindu_synodic_month <- 29 + 7087771/13358334
# TYPE rational
# Mean time from new moon to new moon.

hindu_anomalistic_year <- 1577917828000 / (4320000000 - 387)
# TYPE rational
# Time from aphelion to aphelion.

hindu_anomalistic_month <- 1577917828 / (57753336 - 488199)
# TYPE rational
# Time from apogee to apogee, with bija correction.

hindu_true_position <- function(tee, period, size, anomalistic, change) {
  # TYPE (rational-moment rational rational rational
  # TYPE  rational) -> rational-angle
  # Longitudinal position at moment $tee$.  $period$ is
  # period of mean motion in days.  $size$ is ratio of
  # radii of epicycle and deferent.  $anomalistic$ is the
  # period of retrograde revolution about epicycle.
  # $change$ is maximum decrease in epicycle size.
  lambda <- hindu_mean_position(tee, period) # Position of epicycle center
  offset <- hindu_sine(hindu_mean_position(tee, anomalistic)) # Sine of anomaly
  contraction <- abs(offset) * change * size
  equation <- hindu_arcsin(offset * (size - contraction)) # Equation of center
  (lambda - equation) %% 360
}

hindu_solar_longitude <- function(tee) {
  # TYPE rational-moment -> rational-angle
  # Solar longitude at moment $tee$.
  hindu_true_position(
    tee,
    hindu_sidereal_year,
    14 / 360,
    hindu_anomalistic_year,
    1 / 42
  )
}

hindu_zodiac <- function(tee) {
  # TYPE rational-moment -> hindu-solar-month
  # Zodiacal sign of the sun, as integer in range 1..12,
  # at moment $tee$.
  1 + quotient(hindu_solar_longitude(tee), deg(30))
}

hindu_lunar_longitude <- function(tee) {
  # TYPE rational-moment -> rational-angle
  # Lunar longitude at moment $tee$.
  hindu_true_position(
    tee,
    hindu_sidereal_month,
    32 / 360,
    hindu_anomalistic_month,
    1 / 96
  )
}

hindu_lunar_phase <- function(tee) {
  # TYPE rational-moment -> rational-angle
  # Longitudinal distance between the sun and moon
  # at moment $tee$.
  (hindu_lunar_longitude(tee) - hindu_solar_longitude(tee)) %% 360
}

hindu_lunar_day_from_moment <- function(tee) {
  # TYPE rational-moment -> hindu-lunar-day
  # Phase of moon (tithi) at moment $tee$, as an integer in
  # the range 1..30.
  1 + quotient(hindu_lunar_phase(tee), deg(12))
}

hindu_new_moon_before <- function(tee) {
  # TYPE rational-moment -> rational-moment
  # Approximate moment of last new moon preceding moment
  # $tee$, close enough to determine zodiacal sign.
  varepsilon <- 2^(-1000) # Safety margin.
  tau <- tee - (1 / deg(360)) * hindu_lunar_phase(tee) * hindu_synodic_month # Can be off by almost a day.
  binary_search(
    # Search for phase start.
    l = tau - 1,
    u = min(tee, tau + 1),
    x = hindu_lunar_phase(x) < deg(180),
    hindu_zodiac(l) == hindu_zodiac(u) ||
      u - l < varepsilon
  )
}

hindu_lunar_day_at_or_after <- function(k, tee) {
  # TYPE (rational rational-moment) -> rational-moment
  # Time lunar-day (tithi) number $k$ begins at or after
  # moment $tee$.  $k$ can be fractional (for karanas).
  phase <- (k - 1) * deg(12) # Degrees corresponding to k.
  tau <- tee +
    (1 / deg(360)) * # Mean occurrence of lunar-day.
      ((phase - hindu_lunar_phase(tee)) %% 360) *
      hindu_synodic_month
  a <- max(tee, tau - 2)
  b <- tau + 2
  invert_angular(hindu_lunar_phase, phase, interval_closed(a, b))
}

hindu_calendar_year <- function(tee) {
  # TYPE rational-moment -> hindu-solar-year
  # Determine solar year at given moment $tee$.
  round(
    (tee - hindu_epoch) /
      hindu_sidereal_year -
      hindu_solar_longitude(tee) / deg(360)
  )
}

hindu_solar_era <- 3179
# TYPE standard-year
# Years from Kali Yuga until Saka era.

hindu_solar_from_fixed <- function(date) {
  # TYPE fixed-date -> hindu-solar-date
  # Hindu (Orissa) solar date equivalent to fixed $date$.
  critical <- hindu_sunrise(date + 1) # Sunrise on Hindu date.
  month <- hindu_zodiac(critical)
  year <- hindu_calendar_year(critical) - hindu_solar_era
  approx <- date -
    3 - # 3 days before start of mean month.
    (floor(hindu_solar_longitude(critical)) %% deg(30))
  start <- next(
    i,
    approx, # Search forward for beginning...
    hindu_zodiac(hindu_sunrise(i + 1)) == month
  ) # ... of month.
  day <- date - start + 1
  hindu_solar_date(year, month, day)
}

fixed_from_hindu_solar <- function(s_date) {
  # TYPE hindu-solar-date -> fixed-date
  # Fixed date corresponding to Hindu solar date $s-date$
  # (Saka era; Orissa rule.)
  month <- standard_month(s_date)
  day <- standard_day(s_date)
  year <- standard_year(s_date)
  start <- floor(
    (year +
      hindu_solar_era + # Approximate start of month
      (month - 1) / 12) * # by adding days...
      hindu_sidereal_year
  ) + # in months...
    hindu_epoch # ... and years and days before RD 0.
  # Search forward to correct month
  day - 1 + next(d, start - 3, hindu_zodiac(hindu_sunrise(d + 1)) == month)
}

hindu_lunar_era <- 3044
# TYPE standard-year
# Years from Kali Yuga until Vikrama era.

hindu_lunar_from_fixed <- function(date) {
  # TYPE fixed-date -> hindu-lunar-date
  # Hindu lunar date, new-moon scheme,
  # equivalent to fixed $date$.
  critical <- hindu_sunrise(date) # Sunrise that day.
  day <- hindu_lunar_day_from_moment(critical) # Day of month.
  leap_day <- day ==
    hindu_lunar_day_from_moment(
      # If previous day the same.
      hindu_sunrise(date - 1)
    )
  last_new_moon <- hindu_new_moon_before(critical)
  next_new_moon <- hindu_new_moon_before(
    floor(last_new_moon) + 35
  )
  solar_month <- hindu_zodiac(last_new_moon) # Solar month name.
  leap_month <- solar_month == hindu_zodiac(next_new_moon) # If begins and ends in same sign.
  month <- amod(solar_month + 1, 12) # Month of lunar year.
  year <- hindu_calendar_year(
    # Solar year at end of month.
    if (month <= 2) date + 180 else date # $date$ might precede solar new year.
  ) -
    hindu_lunar_era
  hindu_lunar_date(year, month, leap_month, day, leap_day)
}

fixed_from_hindu_lunar <- function(l_date) {
  # TYPE hindu-lunar-date -> fixed-date
  # Fixed date corresponding to Hindu lunar date $l-date$.
  year <- hindu_lunar_year(l_date)
  month <- hindu_lunar_month(l_date)
  leap_month <- hindu_lunar_leap_month(l_date)
  day <- hindu_lunar_day(l_date)
  leap_day <- hindu_lunar_leap_day(l_date)
  approx <- hindu_epoch +
    hindu_sidereal_year *
      (year + hindu_lunar_era + (month - 1) / 12)
  s <- floor(
    approx -
      hindu_sidereal_year *
        mod3(
          (hindu_solar_longitude(approx) / deg(360)) -
            (month - 1) / 12,
          -1 / 2,
          1 / 2
        )
  )
  k <- hindu_lunar_day_from_moment(s + hr(6))
  est <- s -
    (-day) -
    if (3 < k && k < 27) {
      # Not borderline case.
      k
    } else if (
      {
        mid <- hindu_lunar_from_fixed(s - 15) # Middle of preceding solar month.
        hindu_lunar_month(mid) != month || # In month starting near $s$.
          (hindu_lunar_leap_month(mid) && !leap_month)
      }
    ) {
      mod3(k, -15, 15)
    } else {
      # In preceding month.
      mod3(k, 15, 45)
    }
  tau <- est -
    mod3(
      # Refined estimate.
      hindu_lunar_day_from_moment(est + hr(6)) - day,
      -15,
      15
    )
  date <- next(
    d,
    tau - 1,
    hindu_lunar_day_from_moment(
      hindu_sunrise(d)
    ) %in%
      c(day, amod(day + 1, 30))
  )
  if (leap_day) date + 1 else date
}

hindu_equation_of_time <- function(date) {
  # TYPE fixed-date -> rational-moment
  # Time from true to mean midnight of $date$.
  # (This is a gross approximation to the correct value.)
  offset <- hindu_sine(
    hindu_mean_position(date, hindu_anomalistic_year)
  )
  equation_sun <- offset *
    angle(57, 18, 0) * # Sun's equation of center
    (14 / 360 - abs(offset) / 1080) # Arcsin is not needed since small
  (hindu_daily_motion(date) / deg(360)) *
    (equation_sun / deg(360)) *
    hindu_sidereal_year
}

hindu_ascensional_difference <- function(date, location) {
  # TYPE (fixed-date location) -> rational-angle
  # Difference between right and oblique ascension
  # of sun on $date$ at $location$.
  sin_delta <- 1397 /
    3438 * # Sine of inclination.
    hindu_sine(hindu_tropical_longitude(date))
  phi <- latitude(location)
  diurnal_radius <- hindu_sine(deg(90) + hindu_arcsin(sin_delta))
  tan_phi <- hindu_sine(phi) / # Tangent of latitude as rational number.
    hindu_sine(deg(90) + phi)
  earth_sine <- sin_delta * tan_phi
  hindu_arcsin(-(earth_sine / diurnal_radius))
}

hindu_tropical_longitude <- function(date) {
  # TYPE fixed-date -> rational-angle
  # Hindu tropical longitude on fixed $date$.
  # Assumes precession with maximum of 27 degrees
  # and period of 7200 sidereal years
  # (= 1577917828/600 days).
  days <- date - hindu_epoch # Whole days.
  precession <- deg(27) -
    abs(
      deg(108) *
        mod3(600 / 1577917828 * days - 1 / 4, -1 / 2, 1 / 2)
    )
  (hindu_solar_longitude(date) - precession) %% 360
}

hindu_rising_sign <- function(date) {
  # TYPE fixed-date -> rational-amplitude
  # Tabulated speed of rising of current zodiacal sign on
  # $date$.
  i <- quotient(hindu_tropical_longitude(date), deg(30)) # Index.
  c(
    1670 / 1800,
    1795 / 1800,
    1935 / 1800,
    1935 / 1800,
    1795 / 1800,
    1670 / 1800
  )[(i %% 6) + 1]
}

hindu_daily_motion <- function(date) {
  # TYPE fixed-date -> rational-angle
  # Sidereal daily motion of sun on $date$.
  mean_motion <- deg(360) / hindu_sidereal_year # Mean daily motion in degrees.
  anomaly <- hindu_mean_position(date, hindu_anomalistic_year)
  epicycle <- 14 / 360 - abs(hindu_sine(anomaly)) / 1080 # Current size of epicycle.
  entry <- quotient(anomaly, angle(0, 225, 0))
  sine_table_step <- hindu_sine_table(entry + 1) - # Marginal change in anomaly
    hindu_sine_table(entry)
  factor <- -3438 / 225 * sine_table_step * epicycle
  mean_motion * (1 + factor)
}

hindu_solar_sidereal_difference <- function(date) {
  # TYPE fixed-date -> rational-angle
  # Difference between solar and sidereal day on $date$.
  hindu_daily_motion(date) * hindu_rising_sign(date)
}

ujjain <- location(angle(23, 9, 0), angle(75, 46, 6), mt(0), hr(5 + 461 / 9000))
# TYPE location
# Location of Ujjain.

hindu_location <- ujjain
# TYPE location
# Location (Ujjain) for determining Hindu calendar.

hindu_sunrise <- function(date) {
  # TYPE fixed-date -> rational-moment
  # Sunrise at hindu-location on $date$.
  date +
    hr(6) + # Mean sunrise.
    (longitude(ujjain) - longitude(hindu_location)) /
      deg(360) + # Difference from longitude.
    (-hindu_equation_of_time(date)) + # Apparent midnight.
    (1577917828 / 1582237828 / deg(360)) * # Convert sidereal angle to fraction of civil day.
      (hindu_ascensional_difference(date, hindu_location) +
        1 / 4 * hindu_solar_sidereal_difference(date))
}

hindu_fullmoon_from_fixed <- function(date) {
  # TYPE fixed-date -> hindu-lunar-date
  # Hindu lunar date, full-moon scheme,
  # equivalent to fixed $date$.
  l_date <- hindu_lunar_from_fixed(date)
  year <- hindu_lunar_year(l_date)
  month <- hindu_lunar_month(l_date)
  leap_month <- hindu_lunar_leap_month(l_date)
  day <- hindu_lunar_day(l_date)
  leap_day <- hindu_lunar_leap_day(l_date)
  m <- if (day >= 16) {
    hindu_lunar_month(
      hindu_lunar_from_fixed(date + 20)
    )
  } else {
    month
  }
  hindu_lunar_date(year, m, leap_month, day, leap_day)
}

hindu_expunged <- function(l_year, l_month) {
  # TYPE (hindu-lunar-year hindu-lunar-month) ->
  # TYPE  boolean
  # True of Hindu lunar month $l-month$ in $l-year$
  # is expunged.
  l_month !=
    hindu_lunar_month(
      hindu_lunar_from_fixed(
        fixed_from_hindu_lunar(
          list(l_year, l_month, FALSE, 15, FALSE)
        )
      )
    )
}

fixed_from_hindu_fullmoon <- function(l_date) {
  # TYPE hindu-lunar-date -> fixed-date
  # Fixed date equivalent to Hindu lunar $l-date$
  # in full-moon scheme.
  year <- hindu_lunar_year(l_date)
  month <- hindu_lunar_month(l_date)
  leap_month <- hindu_lunar_leap_month(l_date)
  day <- hindu_lunar_day(l_date)
  leap_day <- hindu_lunar_leap_day(l_date)
  m <- if (leap_month || day <= 15) {
    month
  } else if (hindu_expunged(year, amod(month - 1, 12))) {
    amod(month - 2, 12)
  } else {
    amod(month - 1, 12)
  }
  fixed_from_hindu_lunar(
    hindu_lunar_date(year, m, leap_month, day, leap_day)
  )
}

alt_hindu_sunrise <- function(date) {
  # TYPE fixed-date -> rational-moment
  # Astronomical sunrise at Hindu location on $date$,
  # per Lahiri,
  # rounded to nearest minute, as a rational number.
  rise <- dawn(date, hindu_location, angle(0, 47, 0))
  1 / 24 * 1 / 60 * round(rise * 24 * 60)
}

hindu_sunset <- function(date) {
  # TYPE fixed-date -> rational-moment
  # Sunset at hindu-location on $date$.
  date +
    hr(18) + # Mean sunset.
    (longitude(ujjain) - longitude(hindu_location)) /
      deg(360) + # Difference from longitude.
    (-hindu_equation_of_time(date)) + # Apparent midnight.
    (1577917828 / 1582237828 / deg(360)) * # Convert sidereal angle to fraction of civil day.
      ((-hindu_ascensional_difference(date, hindu_location)) +
        3 / 4 * hindu_solar_sidereal_difference(date))
}

hindu_standard_from_sundial <- function(tee) {
  # TYPE rational-moment -> rational-moment
  # Hindu local time of temporal moment $tee$.
  date <- fixed_from_moment(tee)
  time <- time_from_moment(tee)
  q <- floor(4 * time) # quarter of day
  a <- if (q == 0) {
    # early this morning
    hindu_sunset(date - 1)
  } else if (q == 3) {
    # this evening
    hindu_sunset(date)
  } else {
    # daytime today
    hindu_sunrise(date)
  }
  b <- if (q == 0) {
    hindu_sunrise(date)
  } else if (q == 3) {
    hindu_sunrise(date + 1)
  } else {
    hindu_sunset(date)
  }
  a +
    2 *
      (b - a) *
      (time -
        if (q == 3) {
          hr(18)
        } else if (q == 0) {
          hr(-6)
        } else {
          hr(6)
        })
}

ayanamsha <- function(tee) {
  # TYPE moment -> angle
  # Difference between tropical and sidereal solar longitude.
  solar_longitude(tee) - sidereal_solar_longitude(tee)
}

astro_hindu_sunset <- function(date) {
  # TYPE fixed-date -> moment
  # Geometrical sunset at Hindu location on $date$.
  dusk(date, hindu_location, deg(0))
}

sidereal_zodiac <- function(tee) {
  # TYPE moment -> hindu-solar-month
  # Sidereal zodiacal sign of the sun, as integer in range
  # 1..12, at moment $tee$.
  1 + quotient(sidereal_solar_longitude(tee), deg(30))
}

astro_hindu_calendar_year <- function(tee) {
  # TYPE moment -> hindu-solar-year
  # Astronomical Hindu solar year KY at given moment $tee$.
  round(
    (tee - hindu_epoch) /
      mean_sidereal_year -
      sidereal_solar_longitude(tee) / deg(360)
  )
}

astro_hindu_solar_from_fixed <- function(date) {
  # TYPE fixed-date -> hindu-solar-date
  # Astronomical Hindu (Tamil) solar date equivalent to
  # fixed $date$.
  critical <- astro_hindu_sunset(date) # Sunrise on Hindu date.
  month <- sidereal_zodiac(critical)
  year <- astro_hindu_calendar_year(critical) - hindu_solar_era
  approx <- date -
    3 - # 3 days before start of mean month.
    (floor(sidereal_solar_longitude(critical)) %% deg(30))
  start <- next(
    i,
    approx, # Search forward for beginning...
    sidereal_zodiac(astro_hindu_sunset(i)) == month
  ) # ... of month.
  day <- date - start + 1
  hindu_solar_date(year, month, day)
}

fixed_from_astro_hindu_solar <- function(s_date) {
  # TYPE hindu-solar-date -> fixed-date
  # Fixed date corresponding to Astronomical
  # Hindu solar date (Tamil rule; Saka era).
  month <- standard_month(s_date)
  day <- standard_day(s_date)
  year <- standard_year(s_date)
  approx <- hindu_epoch -
    3 + # 3 days before start of mean month.
    floor(
      (year + hindu_solar_era + (month - 1) / 12) *
        mean_sidereal_year
    )
  start <- next(
    i,
    approx, # Search forward for beginning...
    sidereal_zodiac(astro_hindu_sunset(i)) == month
  ) # ... of month.
  start + day - 1
}

astro_lunar_day_from_moment <- function(tee) {
  # TYPE moment -> hindu-lunar-day
  # Phase of moon (tithi) at moment $tee$, as an integer in
  # the range 1..30.
  1 + quotient(lunar_phase(tee), deg(12))
}

astro_hindu_lunar_from_fixed <- function(date) {
  # TYPE fixed-date -> hindu-lunar-date
  # Astronomical Hindu lunar date equivalent to fixed $date$.
  critical <- alt_hindu_sunrise(date) # Sunrise that day.
  day <- astro_lunar_day_from_moment(critical) # Day of month
  leap_day <- day ==
    astro_lunar_day_from_moment(
      # If previous day the same.
      alt_hindu_sunrise(date - 1)
    )
  last_new_moon <- new_moon_before(critical)
  next_new_moon <- new_moon_at_or_after(critical)
  solar_month <- sidereal_zodiac(last_new_moon) # Solar month name.
  leap_month <- solar_month == sidereal_zodiac(next_new_moon) # If begins and ends in same sign.
  month <- amod(solar_month + 1, 12) # Month of lunar year.
  year <- astro_hindu_calendar_year(
    # Solar year at end of month.
    if (month <= 2) date + 180 else date # $date$ might precede solar new year.
  ) -
    hindu_lunar_era
  hindu_lunar_date(year, month, leap_month, day, leap_day)
}

fixed_from_astro_hindu_lunar <- function(l_date) {
  # TYPE hindu-lunar-date -> fixed-date
  # Fixed date corresponding to Hindu lunar date $l-date$.
  year <- hindu_lunar_year(l_date)
  month <- hindu_lunar_month(l_date)
  leap_month <- hindu_lunar_leap_month(l_date)
  day <- hindu_lunar_day(l_date)
  leap_day <- hindu_lunar_leap_day(l_date)
  approx <- hindu_epoch +
    mean_sidereal_year *
      (year + hindu_lunar_era + (month - 1) / 12)
  s <- floor(
    approx -
      hindu_sidereal_year *
        mod3(
          (sidereal_solar_longitude(approx) / deg(360)) -
            (month - 1) / 12,
          -1 / 2,
          1 / 2
        )
  )
  k <- astro_lunar_day_from_moment(s + hr(6))
  est <- s -
    (-day) -
    if (3 < k && k < 27) {
      # Not borderline case.
      k
    } else if (
      {
        mid <- astro_hindu_lunar_from_fixed(s - 15) # Middle of preceding solar month.
        hindu_lunar_month(mid) != month || # In month starting near $s$.
          (hindu_lunar_leap_month(mid) && !leap_month)
      }
    ) {
      mod3(k, -15, 15)
    } else {
      # In preceding month.
      mod3(k, 15, 45)
    }
  tau <- est -
    mod3(
      # Refined estimate.
      astro_lunar_day_from_moment(est + hr(6)) - day,
      -15,
      15
    )
  date <- next(
    d,
    tau - 1,
    astro_lunar_day_from_moment(
      alt_hindu_sunrise(d)
    ) %in%
      c(day, amod(day + 1, 30))
  )
  if (leap_day) date + 1 else date
}

hindu_lunar_station <- function(date) {
  # TYPE fixed-date -> nakshatra
  # Hindu lunar station (nakshatra) at sunrise on $date$.
  critical <- hindu_sunrise(date)
  1 + quotient(hindu_lunar_longitude(critical), angle(0, 800, 0))
}

hindu_solar_longitude_at_or_after <- function(lambda, tee) {
  # TYPE (season moment) -> moment
  # Moment of the first time at or after $tee$
  # when Hindu solar longitude will be $lambda$ degrees.
  tau <- tee + # Estimate (within 5 days).
    hindu_sidereal_year *
      (1 / deg(360)) *
      ((lambda - hindu_solar_longitude(tee)) %% 360)
  a <- max(tee, tau - 5) # At or after tee.
  b <- tau + 5
  invert_angular(hindu_solar_longitude, lambda, interval_closed(a, b))
}

mesha_samkranti <- function(g_year) {
  # TYPE gregorian-year -> rational-moment
  # Fixed moment of Mesha samkranti (Vernal equinox)
  # in Gregorian $g-year$.
  jan1 <- gregorian_new_year(g_year)
  hindu_solar_longitude_at_or_after(deg(0), jan1)
}

sidereal_start <- precession(universal_from_local(
  mesha_samkranti(ce(285)),
  hindu_location
))
# TYPE angle

hindu_lunar_new_year <- function(g_year) {
  # TYPE gregorian-year -> fixed-date
  # Fixed date of Hindu lunisolar new year in Gregorian
  # $g-year$.
  jan1 <- gregorian_new_year(g_year)
  mina <- hindu_solar_longitude_at_or_after(deg(330), jan1) # Fixed moment of solar longitude 330.
  new_moon <- hindu_lunar_day_at_or_after(1, mina) # Next new moon.
  h_day <- floor(new_moon)
  critical <- hindu_sunrise(h_day) # Sunrise that day.
  h_day +
    # Next day if new moon after sunrise,
    # unless lunar day ends before next sunrise.
    if (
      new_moon < critical ||
        hindu_lunar_day_from_moment(
          hindu_sunrise(h_day + 1)
        ) ==
          2
    ) {
      0
    } else {
      1
    }
}

hindu_lunar_on_or_before <- function(l_date1, l_date2) {
  # TYPE (hindu-lunar-date hindu-lunar-date) -> boolean
  # True if Hindu lunar date $l-date1$ is on or before
  # Hindu lunar date $l-date2$.
  month1 <- hindu_lunar_month(l_date1)
  month2 <- hindu_lunar_month(l_date2)
  leap1 <- hindu_lunar_leap_month(l_date1)
  leap2 <- hindu_lunar_leap_month(l_date2)
  day1 <- hindu_lunar_day(l_date1)
  day2 <- hindu_lunar_day(l_date2)
  leap_day1 <- hindu_lunar_leap_day(l_date1)
  leap_day2 <- hindu_lunar_leap_day(l_date2)
  year1 <- hindu_lunar_year(l_date1)
  year2 <- hindu_lunar_year(l_date2)
  year1 < year2 ||
    (year1 == year2 &&
      (month1 < month2 ||
        (month1 == month2 &&
          ((leap1 && !leap2) ||
            (identical(leap1, leap2) &&
              (day1 < day2 ||
                (day1 == day2 &&
                  (!leap_day1 || leap_day2))))))))
}

hindu_date_occur <- function(l_year, l_month, l_day) {
  # TYPE (hindu-lunar-year hindu-lunar-month
  # TYPE  hindu-lunar-day) -> fixed-date
  # Fixed date of occurrence of Hindu lunar $l-month$,
  # $l-day$ in Hindu lunar year $l-year$, taking leap and
  # expunged days into account.  When the month is
  # expunged, then the following month is used.
  lunar <- hindu_lunar_date(l_year, l_month, FALSE, l_day, FALSE)
  try <- fixed_from_hindu_lunar(lunar)
  mid <- hindu_lunar_from_fixed(
    if (l_day > 15) try - 5 else try
  )
  expunged <- l_month != hindu_lunar_month(mid)
  l_date <- hindu_lunar_date(
    # day in next month
    hindu_lunar_year(mid),
    hindu_lunar_month(mid),
    hindu_lunar_leap_month(mid),
    l_day,
    FALSE
  )
  if (expunged) {
    next(
      d,
      try,
      !hindu_lunar_on_or_before(
        hindu_lunar_from_fixed(d),
        l_date
      )
    ) -
      1
  } else if (
    l_day !=
      hindu_lunar_day(
        hindu_lunar_from_fixed(try)
      )
  ) {
    try - 1
  } else {
    try
  }
}

hindu_lunar_holiday <- function(l_month, l_day, g_year) {
  # TYPE (hindu-lunar-month hindu-lunar-day
  # TYPE  gregorian-year) -> list-of-fixed-dates
  # List of fixed dates of occurrences of Hindu lunar
  # $month$, $day$ in Gregorian year $g-year$.
  l_year <- hindu_lunar_year(
    hindu_lunar_from_fixed(
      gregorian_new_year(g_year)
    )
  )
  date0 <- hindu_date_occur(l_year, l_month, l_day)
  date1 <- hindu_date_occur(l_year + 1, l_month, l_day)
  list_range(list(date0, date1), gregorian_year_range(g_year))
}

diwali <- function(g_year) {
  # TYPE gregorian-year -> list-of-fixed-dates
  # List of fixed date(s) of Diwali in Gregorian year
  # $g-year$.
  hindu_lunar_holiday(8, 1, g_year)
}

hindu_tithi_occur <- function(l_month, tithi, tee, l_year) {
  # TYPE (hindu-lunar-month rational rational
  # TYPE  hindu-lunar-year) -> fixed-date
  # Fixed date of occurrence of Hindu lunar $tithi$ prior
  # to sundial time $tee$, in Hindu lunar $l-month$, $l-year$.
  approx <- hindu_date_occur(l_year, l_month, floor(tithi))
  lunar <- hindu_lunar_day_at_or_after(tithi, approx - 2)
  try <- fixed_from_moment(lunar)
  tee_h <- standard_from_sundial(try + tee, ujjain)
  if (
    lunar <= tee_h ||
      hindu_lunar_phase(
        standard_from_sundial(try + 1 + tee, ujjain)
      ) >
        12 * tithi
  ) {
    try
  } else {
    try + 1
  }
}

hindu_lunar_event <- function(l_month, tithi, tee, g_year) {
  # TYPE (hindu-lunar-month rational rational
  # TYPE  gregorian-year) -> list-of-fixed-dates
  # List of fixed dates of occurrences of Hindu lunar $tithi$
  # prior to sundial time $tee$, in Hindu lunar $l-month$,
  # in Gregorian year $g-year$.
  l_year <- hindu_lunar_year(
    hindu_lunar_from_fixed(
      gregorian_new_year(g_year)
    )
  )
  date0 <- hindu_tithi_occur(l_month, tithi, tee, l_year)
  date1 <- hindu_tithi_occur(l_month, tithi, tee, l_year + 1)
  list_range(list(date0, date1), gregorian_year_range(g_year))
}

shiva <- function(g_year) {
  # TYPE gregorian-year -> list-of-fixed-dates
  # List of fixed date(s) of Night of Shiva in Gregorian
  # year $g-year$.
  hindu_lunar_event(11, 29, hr(24), g_year)
}

rama <- function(g_year) {
  # TYPE gregorian-year -> list-of-fixed-dates
  # List of fixed date(s) of Rama's Birthday in Gregorian
  # year $g-year$.
  hindu_lunar_event(1, 9, hr(12), g_year)
}

karana <- function(n) {
  # TYPE 1-60 -> 0-10
  # Number (0-10) of the name of the $n$-th (1-60) Hindu
  # karana.
  if (n == 1) {
    0
  } else if (n > 57) {
    n - 50
  } else {
    amod(n - 1, 7)
  }
}

yoga <- function(date) {
  # TYPE fixed-date -> 1-27
  # Hindu yoga on $date$.
  1 + floor(((hindu_solar_longitude(date) +
              hindu_lunar_longitude(date)) /
             angle(0, 800, 0)) %% 27)
}

sacred_wednesdays <- function(g_year) {
  # TYPE gregorian-year -> list-of-fixed-dates
  # List of Wednesdays in Gregorian year $g-year$
  # that are day 8 of Hindu lunar months.
  sacred_wednesdays_in_range(
    gregorian_year_range(g_year)
  )
}

sacred_wednesdays_in_range <- function(range) {
  # TYPE range -> list-of-fixed-dates
  # List of Wednesdays within $range$ of dates
  # that are day 8 of Hindu lunar months.
  a <- begin(range)
  b <- end(range)
  wed <- kday_on_or_after(wednesday, a)
  h_date <- hindu_lunar_from_fixed(wed)
  if (in_range(wed, range)) {
    c(
      if (hindu_lunar_day(h_date) == 8) {
        list(wed)
      } else {
        NULL
      },
      sacred_wednesdays_in_range(
        interval(wed + 1, b)
      )
    )
  } else {
    NULL
  }
}
