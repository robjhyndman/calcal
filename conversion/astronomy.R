#==============================================================================
# Time and Astronomy Functions (Part 1)
#==============================================================================

hr <- function(x) {
  x / 24
}

mn <- function(x) {
  x / (24 * 60)
}

sec <- function(x) {
  x / (24 * 60 * 60)
}

mt <- function(x) {
  x # For typesetting purposes
}

deg <- function(x) {
  x # For typesetting purposes
}

mins <- function(x) {
  x / 60
}

secs <- function(x) {
  x / 3600
}

angle <- function(d, m, s) {
  d + m / 60 + s / 3600
}

# Trigonometric functions for degrees
degrees_from_radians <- function(theta) {
  (theta * 180 / pi) %% 360
}

radians_from_degrees <- function(theta) {
  (theta %% 360) * pi / 180
}

sin_degrees <- function(theta) {
  sin(radians_from_degrees(theta))
}

cos_degrees <- function(theta) {
  cos(radians_from_degrees(theta))
}

tan_degrees <- function(theta) {
  tan(radians_from_degrees(theta))
}

arctan_degrees <- function(y, x) {
  if (x == 0 && y == 0) {
    return(BOGUS)
  }

  result <- if (x == 0) {
    sign(y) * 90
  } else {
    alpha <- degrees_from_radians(atan(y / x))
    if (x >= 0) alpha else alpha + 180
  }

  result %% 360
}

arcsin_degrees <- function(x) {
  degrees_from_radians(asin(x))
}

arccos_degrees <- function(x) {
  degrees_from_radians(acos(x))
}

#==============================================================================
# Time and Astronomy Functions (Part 2)
#==============================================================================

# Location definition
location <- function(latitude, longitude, elevation, zone) {
  list(
    latitude = latitude,
    longitude = longitude,
    elevation = elevation,
    zone = zone
  )
}

latitude <- function(loc) {
  loc$latitude
}

longitude <- function(loc) {
  loc$longitude
}

elevation <- function(loc) {
  loc$elevation
}

zone <- function(loc) {
  loc$zone
}

# Locations
MECCA <- location(angle(21, 25, 24), angle(39, 49, 24), mt(298), hr(3))
JERUSALEM <- location(angle(31.78, 0, 0), angle(35.24, 0, 0), mt(740), hr(2))
TEHRAN <- location(angle(35.68, 0, 0), angle(51.42, 0, 0), mt(1100), hr(3.5))
BABYLON <- location(angle(32.4794, 0, 0), angle(44.4328, 0, 0), mt(26), hr(3.5))
UJJAIN <- location(angle(23, 9, 0), angle(75, 46, 6), mt(0), hr(5 + 461 / 9000))

# Time conversion functions
standard_from_universal <- function(tee_rom_u, loc) {
  tee_rom_u + zone(loc)
}

universal_from_standard <- function(tee_rom_s, loc) {
  tee_rom_s - zone(loc)
}

zone_from_longitude <- function(phi) {
  phi / 360
}

local_from_universal <- function(tee_rom_u, loc) {
  tee_rom_u + zone_from_longitude(longitude(loc))
}

universal_from_local <- function(tee_ell, loc) {
  tee_ell - zone_from_longitude(longitude(loc))
}

standard_from_local <- function(tee_ell, loc) {
  standard_from_universal(universal_from_local(tee_ell, loc), loc)
}

local_from_standard <- function(tee_rom_s, loc) {
  local_from_universal(universal_from_standard(tee_rom_s, loc), loc)
}

apparent_from_local <- function(tee_ell, loc) {
  tee_ell + equation_of_time(universal_from_local(tee_ell, loc))
}

local_from_apparent <- function(tee, loc) {
  tee - equation_of_time(universal_from_local(tee, loc))
}

apparent_from_universal <- function(tee_rom_u, loc) {
  apparent_from_local(local_from_universal(tee_rom_u, loc), loc)
}

universal_from_apparent <- function(tee, loc) {
  universal_from_local(local_from_apparent(tee, loc), loc)
}

midnight <- function(date, loc) {
  universal_from_apparent(date, loc)
}

midday <- function(date, loc) {
  universal_from_apparent(date + hr(12), loc)
}

# Astronomical functions for Julian centuries
J2000 <- hr(12) + gregorian_new_year(2000) # Noon at start of Gregorian year 2000

julian_centuries <- function(tee) {
  (dynamical_from_universal(tee) - J2000) / 36525
}

obliquity <- function(tee) {
  c <- julian_centuries(tee)
  angle(23, 26, 21.448) +
    poly(
      c,
      c(0, angle(0, 0, -46.815), angle(0, 0, -0.00059), angle(0, 0, 0.001813))
    )
}

declination <- function(tee, beta, lambda) {
  varepsilon <- obliquity(tee)
  arcsin_degrees(
    sin_degrees(beta) *
      cos_degrees(varepsilon) +
      cos_degrees(beta) * sin_degrees(varepsilon) * sin_degrees(lambda)
  )
}

right_ascension <- function(tee, beta, lambda) {
  varepsilon <- obliquity(tee)
  arctan_degrees(
    sin_degrees(lambda) *
      cos_degrees(varepsilon) -
      tan_degrees(beta) * sin_degrees(varepsilon),
    cos_degrees(lambda)
  )
}

sine_offset <- function(tee, loc, alpha) {
  phi <- latitude(loc)
  tee_prime <- universal_from_local(tee, loc)
  delta <- declination(tee_prime, 0, solar_longitude(tee_prime))

  tan_degrees(phi) *
    tan_degrees(delta) +
    sin_degrees(alpha) / (cos_degrees(delta) * cos_degrees(phi))
}

approx_moment_of_depression <- function(tee, loc, alpha, early) {
  try_val <- sine_offset(tee, loc, alpha)
  date <- fixed_from_moment(tee)
  alt <- if (alpha >= 0) {
    if (early) date else date + 1
  } else {
    date + hr(12)
  }
  value <- if (abs(try_val) > 1) sine_offset(alt, loc, alpha) else try_val

  if (abs(value) <= 1) {
    # Event occurs
    offset <- mod3(arcsin_degrees(value) / 360, hr(-12), hr(12))
    local_from_apparent(
      date + if (early) hr(6) - offset else hr(18) + offset,
      loc
    )
  } else {
    BOGUS
  }
}

moment_of_depression <- function(approx, loc, alpha, early) {
  tee <- approx_moment_of_depression(approx, loc, alpha, early)

  if (tee == BOGUS) {
    BOGUS
  } else if (abs(approx - tee) < sec(30)) {
    tee
  } else {
    moment_of_depression(tee, loc, alpha, early)
  }
}

MORNING <- TRUE
EVENING <- FALSE

dawn <- function(date, loc, alpha) {
  result <- moment_of_depression(date + hr(6), loc, alpha, MORNING)

  if (result == BOGUS) {
    BOGUS
  } else {
    standard_from_local(result, loc)
  }
}

dusk <- function(date, loc, alpha) {
  result <- moment_of_depression(date + hr(18), loc, alpha, EVENING)

  if (result == BOGUS) {
    BOGUS
  } else {
    standard_from_local(result, loc)
  }
}

refraction <- function(tee, loc) {
  h <- max(mt(0), elevation(loc))
  cap_R <- mt(6.372e6) # Radius of Earth in meters
  dip <- arccos_degrees(cap_R / (cap_R + h)) # Depression of visible horizon

  mins(34) + dip + secs(19) * sqrt(h)
}

sunrise <- function(date, loc) {
  alpha <- refraction(date + hr(6), loc) + mins(16)
  dawn(date, loc, alpha)
}

sunset <- function(date, loc) {
  alpha <- refraction(date + hr(18), loc) + mins(16)
  dusk(date, loc, alpha)
}

jewish_dusk <- function(date, loc) {
  dusk(date, loc, angle(4, 40, 0))
}

jewish_sabbath_ends <- function(date, loc) {
  dusk(date, loc, angle(7, 5, 0))
}

daytime_temporal_hour <- function(date, loc) {
  sunrise_time <- sunrise(date, loc)
  sunset_time <- sunset(date, loc)

  if (sunrise_time == BOGUS || sunset_time == BOGUS) {
    BOGUS
  } else {
    (sunset_time - sunrise_time) / 12
  }
}

nighttime_temporal_hour <- function(date, loc) {
  next_sunrise <- sunrise(date + 1, loc)
  sunset_time <- sunset(date, loc)

  if (next_sunrise == BOGUS || sunset_time == BOGUS) {
    BOGUS
  } else {
    (next_sunrise - sunset_time) / 12
  }
}
