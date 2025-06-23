# Section: Time and Astronomy

hr <- function(x) {
  # TYPE real -> duration
  # x hours.
  x / 24
}

mn <- function(x) {
  # TYPE real -> duration
  # x minutes.
  x / (24 * 60)
}

sec <- function(x) {
  # TYPE real -> duration
  # x seconds.
  x / (24 * 60 * 60)
}

mt <- function(x) {
  # TYPE real -> distance
  # x meters.
  # For typesetting purposes.
  x
}

deg <- function(x) {
  # TYPE real -> angle
  # TYPE list-of-reals -> list-of-angles
  # x degrees.
  # For typesetting purposes.
  x
}

mins <- function(x) {
  # TYPE real -> angle
  # x arcminutes
  x / 60
}

secs <- function(x) {
  # TYPE real -> angle
  # x arcseconds
  x / 3600
}

angle <- function(d, m, s) {
  # TYPE (integer integer real) -> angle
  # d degrees, m arcminutes, s arcseconds.
  d + (m + s/60) / 60
}

degrees_from_radians <- function(theta) {
  # TYPE radian -> angle
  # Convert angle theta from radians to degrees.
  (theta / pi * 180) %% 360
}

radians_from_degrees <- function(theta) {
  # TYPE real -> radian
  # Convert angle theta from degrees to radians.
  (theta %% 360) * pi / 180
}

sin_degrees <- function(theta) {
  # TYPE angle -> amplitude
  # Sine of theta (given in degrees).
  sin(radians_from_degrees(theta))
}

cos_degrees <- function(theta) {
  # TYPE angle -> amplitude
  # Cosine of theta (given in degrees).
  cos(radians_from_degrees(theta))
}

tan_degrees <- function(theta) {
  # TYPE angle -> real
  # Tangent of theta (given in degrees).
  tan(radians_from_degrees(theta))
}

arctan_degrees <- function(y, x) {
  # TYPE (real real) -> angle
  # Arctangent of y/x in degrees.
  # Returns bogus if x and y are both 0.
  if (x == 0 && y == 0) {
    return(bogus)
  }
  
  if (x == 0) {
    alpha <- sign(y) * deg(90)
  } else {
    alpha <- degrees_from_radians(atan(y / x))
    if (x < 0) {
      alpha <- alpha + deg(180)
    }
  }
  
  alpha %% 360
}

arcsin_degrees <- function(x) {
  # TYPE amplitude -> angle
  # Arcsine of x in degrees.
  degrees_from_radians(asin(x))
}

arccos_degrees <- function(x) {
  # TYPE amplitude -> angle
  # Arccosine of x in degrees.
  degrees_from_radians(acos(x))
}

location <- function(latitude, longitude, elevation, zone) {
  # TYPE (half-circle circle distance real) -> location
  list(latitude, longitude, elevation, zone)
}

latitude <- function(location) {
  # TYPE location -> half-circle
  location[[1]]
}

longitude <- function(location) {
  # TYPE location -> circle
  location[[2]]
}

elevation <- function(location) {
  # TYPE location -> distance
  location[[3]]
}

zone <- function(location) {
  # TYPE location -> real
  location[[4]]
}

# Location of Mecca.
MECCA <- location(angle(21, 25, 24), angle(39, 49, 24), mt(298), hr(3))

direction <- function(location, focus) {
  # TYPE (location location) -> angle
  # Angle (clockwise from North) to face focus when
  # standing in location. Subject to errors near focus and
  # its antipode.
  phi <- latitude(location)
  phi_prime <- latitude(focus)
  psi <- longitude(location)
  psi_prime <- longitude(focus)
  y <- sin_degrees(psi_prime - psi)
  x <- cos_degrees(phi) * tan_degrees(phi_prime) - 
       sin_degrees(phi) * cos_degrees(psi - psi_prime)
  
  if ((x == 0 && y == 0) || phi_prime == deg(90)) {
    return(deg(0))
  } else if (phi_prime == deg(-90)) {
    return(deg(180))
  } else {
    return(arctan_degrees(y, x))
  }
}

standard_from_universal <- function(tee_rom_u, location) {
  # TYPE (moment location) -> moment
  # Standard time from tee_rom_u in universal time at location.
  tee_rom_u + zone(location)
}

universal_from_standard <- function(tee_rom_s, location) {
  # TYPE (moment location) -> moment
  # Universal time from tee_rom_s in standard time at location.
  tee_rom_s - zone(location)
}

zone_from_longitude <- function(phi) {
  # TYPE circle -> duration
  # Difference between UT and local mean time at longitude
  # phi as a fraction of a day.
  phi / deg(360)
}

local_from_universal <- function(tee_rom_u, location) {
  # TYPE (moment location) -> moment
  # Local time from universal tee_rom_u at location.
  tee_rom_u + zone_from_longitude(longitude(location))
}

universal_from_local <- function(tee_ell, location) {
  # TYPE (moment location) -> moment
  # Universal time from local tee_ell at location.
  tee_ell - zone_from_longitude(longitude(location))
}

standard_from_local <- function(tee_ell, location) {
  # TYPE (moment location) -> moment
  # Standard time from local tee_ell at location.
  standard_from_universal(universal_from_local(tee_ell, location), location)
}

local_from_standard <- function(tee_rom_s, location) {
  # TYPE (moment location) -> moment
  # Local time from standard tee_rom_s at location.
  local_from_universal(universal_from_standard(tee_rom_s, location), location)
}

apparent_from_local <- function(tee_ell, location) {
  # TYPE (moment location) -> moment
  # Sundial time from local time tee_ell at location.
  tee_ell + equation_of_time(universal_from_local(tee_ell, location))
}

local_from_apparent <- function(tee, location) {
  # TYPE (moment location) -> moment
  # Local time from sundial time tee at location.
  tee - equation_of_time(universal_from_local(tee, location))
}

apparent_from_universal <- function(tee_rom_u, location) {
  # TYPE (moment location) -> moment
  # True (apparent) time at universal time tee at location.
  apparent_from_local(local_from_universal(tee_rom_u, location), location)
}

universal_from_apparent <- function(tee, location) {
  # TYPE (moment location) -> moment
  # Universal time from sundial time tee at location.
  universal_from_local(local_from_apparent(tee, location), location)
}

midnight <- function(date, location) {
  # TYPE (fixed-date location) -> moment
  # Universal time of true (apparent) midnight of fixed date at location.
  universal_from_apparent(date, location)
}

midday <- function(date, location) {
  # TYPE (fixed-date location) -> moment
  # Universal time on fixed date of midday at location.
  universal_from_apparent(date + hr(12), location)
}

julian_centuries <- function(tee) {
  # TYPE moment -> century
  # Julian centuries since 2000 at moment tee.
  (dynamical_from_universal(tee) - J2000) / 36525
}

obliquity <- function(tee) {
  # TYPE moment -> angle
  # Obliquity of ecliptic at moment tee.
  c <- julian_centuries(tee)
  angle(23, 26, 21.448) + 
    poly(c, c(0, angle(0, 0, -46.8150), angle(0, 0, -0.00059), angle(0, 0, 0.001813)))
}

declination <- function(tee, beta, lambda) {
  # TYPE (moment half-circle circle) -> angle
  # Declination at moment UT tee of object at
  # latitude beta and longitude lambda.
  varepsilon <- obliquity(tee)
  arcsin_degrees(sin_degrees(beta) * cos_degrees(varepsilon) +
                 cos_degrees(beta) * sin_degrees(varepsilon) * sin_degrees(lambda))
}

right_ascension <- function(tee, beta, lambda) {
  # TYPE (moment half-circle circle) -> angle
  # Right ascension at moment UT tee of object at
  # latitude beta and longitude lambda.
  varepsilon <- obliquity(tee)
  arctan_degrees(sin_degrees(lambda) * cos_degrees(varepsilon) - 
                 tan_degrees(beta) * sin_degrees(varepsilon),
                 cos_degrees(lambda))
}

sine_offset <- function(tee, location, alpha) {
  # TYPE (moment location half-circle) -> real
  # Sine of angle between position of sun at 
  # local time tee and when its depression is alpha at location.
  # Out of range when it does not occur.
  phi <- latitude(location)
  tee_prime <- universal_from_local(tee, location)
  delta <- declination(tee_prime, deg(0), solar_longitude(tee_prime))
  
  tan_degrees(phi) * tan_degrees(delta) + 
    sin_degrees(alpha) / (cos_degrees(delta) * cos_degrees(phi))
}

approx_moment_of_depression <- function(tee, location, alpha, early_p) {
  # TYPE (moment location half-circle boolean) -> moment
  # Moment in local time near tee when depression angle
  # of sun is alpha (negative if above horizon) at
  # location; early_p is true when morning event is sought
  # and false for evening. Returns bogus if depression
  # angle is not reached.
  try_val <- sine_offset(tee, location, alpha)
  date <- fixed_from_moment(tee)
  
  if (alpha >= 0) {
    alt <- if (early_p) date else (date + 1)
  } else {
    alt <- date + hr(12)
  }
  
  value <- if (abs(try_val) > 1) {
    sine_offset(alt, location, alpha)
  } else {
    try_val
  }
  
  if (abs(value) <= 1) {  # Event occurs
    offset <- mod3(arcsin_degrees(value) / deg(360), hr(-12), hr(12))
    local_from_apparent(
      date + if (early_p) (hr(6) - offset) else (hr(18) + offset),
      location)
  } else {
    bogus
  }
}

moment_of_depression <- function(approx, location, alpha, early_p) {
  # TYPE (moment location half-circle boolean) -> moment
  # Moment in local time near approx when depression
  # angle of sun is alpha (negative if above horizon) at
  # location; early_p is true when morning event is
  # sought, and false for evening.
  # Returns bogus if depression angle is not reached.
  tee <- approx_moment_of_depression(approx, location, alpha, early_p)
  
  if (identical(tee, bogus)) {
    return(bogus)
  }
  
  if (abs(approx - tee) < sec(30)) {
    return(tee)
  } else {
    return(moment_of_depression(tee, location, alpha, early_p))
  }
}

# Signifies morning.
MORNING <- TRUE

# Signifies evening.
EVENING <- FALSE

dawn <- function(date, location, alpha) {
  # TYPE (fixed-date location half-circle) -> moment
  # Standard time in morning on fixed date at
  # location when depression angle of sun is alpha.
  # Returns bogus if there is no dawn on date.
  result <- moment_of_depression(date + hr(6), location, alpha, MORNING)
  
  if (identical(result, bogus)) {
    return(bogus)
  } else {
    return(standard_from_local(result, location))
  }
}

dusk <- function(date, location, alpha) {
  # TYPE (fixed-date location half-circle) -> moment
  # Standard time in evening on fixed date at
  # location when depression angle of sun is alpha.
  # Returns bogus if there is no dusk on date.
  result <- moment_of_depression(date + hr(18), location, alpha, EVENING)
  
  if (identical(result, bogus)) {
    return(bogus)
  } else {
    return(standard_from_local(result, location))
  }
}

refraction <- function(tee, location) {
  # TYPE (moment location) -> half-circle
  # Refraction angle at moment tee at location.
  # The moment is not used.
  h <- max(mt(0), elevation(location))
  cap_R <- mt(6.372e6)  # Radius of Earth.
  dip <- arccos_degrees(cap_R / (cap_R + h))  # Depression of visible horizon.
  
  mins(34) + dip + secs(19) * sqrt(h)
}

sunrise <- function(date, location) {
  # TYPE (fixed-date location) -> moment
  # Standard time of sunrise on fixed date at location.
  alpha <- refraction(date + hr(6), location) + mins(16)
  dawn(date, location, alpha)
}

sunset <- function(date, location) {
  # TYPE (fixed-date location) -> moment
  # Standard time of sunset on fixed date at location.
  alpha <- refraction(date + hr(18), location) + mins(16)
  dusk(date, location, alpha)
}

jewish_dusk <- function(date, location) {
  # TYPE (fixed-date location) -> moment
  # Standard time of Jewish dusk on fixed date
  # at location (as per Vilna Gaon).
  dusk(date, location, angle(4, 40, 0))
}

jewish_sabbath_ends <- function(date, location) {
  # TYPE (fixed-date location) -> moment
  # Standard time of end of Jewish sabbath on fixed date
  # at location (as per Berthold Cohn).
  dusk(date, location, angle(7, 5, 0))
}

daytime_temporal_hour <- function(date, location) {
  # TYPE (fixed-date location) -> real
  # Length of daytime temporal hour on fixed date at location.
  # Returns bogus if there no sunrise or sunset on date.
  sunrise_time <- sunrise(date, location)
  sunset_time <- sunset(date, location)
  
  if (identical(sunrise_time, bogus) || identical(sunset_time, bogus)) {
    return(bogus)
  } else {
    return((sunset_time - sunrise_time) / 12)
  }
}

nighttime_temporal_hour <- function(date, location) {
  # TYPE (fixed-date location) -> real
  # Length of nighttime temporal hour on fixed date at location.
  # Returns bogus if there no sunrise or sunset on date.
  sunrise_next <- sunrise(date + 1, location)
  sunset_time <- sunset(date, location)
  
  if (identical(sunrise_next, bogus) || identical(sunset_time, bogus)) {
    return(bogus)
  } else {
    return((sunrise_next - sunset_time) / 12)
  }
}

standard_from_sundial <- function(tee, location) {
  # TYPE (moment location) -> moment
  # Standard time of temporal moment tee at location.
  # Returns bogus if temporal hour is undefined that day.
  date <- fixed_from_moment(tee)
  hour <- 24 * time_from_moment(tee)
  
  if (hour >= 6 && hour <= 18) {  # daytime today
    h <- daytime_temporal_hour(date, location)
  } else if (hour < 6) {  # early this morning
    h <- nighttime_temporal_hour(date - 1, location)
  } else {  # this evening
    h <- nighttime_temporal_hour(date, location)
  }
  
  if (identical(h, bogus)) {
    return(bogus)
  }
  
  if (hour >= 6 && hour <= 18) {  # daytime today
    return(sunrise(date, location) + (hour - 6) * h)
  } else if (hour < 6) {  # early this morning
    return(sunset(date - 1, location) + (hour + 6) * h)
  } else {  # this evening
    return(sunset(date, location) + (hour - 18) * h)
  }
}

jewish_morning_end <- function(date, location) {
  # TYPE (fixed-date location) -> moment
  # Standard time on fixed date at location of end of
  # morning according to Jewish ritual.
  standard_from_sundial(date + hr(10), location)
}

asr <- function(date, location) {
  # TYPE (fixed-date location) -> moment
  # Standard time of asr on fixed date at location.
  # According to Hanafi rule.
  # Returns bogus is no asr occurs.
  noon <- midday(date, location)  # Time when sun nearest zenith.
  phi <- latitude(location)
  delta <- declination(noon, deg(0), solar_longitude(noon))  # Solar declination at noon.
  altitude <- arcsin_degrees(cos_degrees(delta) * cos_degrees(phi) +
                             sin_degrees(delta) * sin_degrees(phi))  # Solar altitude at noon.
  
  if (altitude <= deg(0)) {  # No shadow.
    return(bogus)
  }
  
  # Sun's altitude when shadow increases by double its length.
  h <- mod3(arctan_degrees(tan_degrees(altitude), 1 + 2 * tan_degrees(altitude)), -90, 90)
  dusk(date, location, -h)
}

alt_asr <- function(date, location) {
  # TYPE (fixed-date location) -> moment
  # Standard time of asr on fixed date at location.
  # According to Shafi'i rule.
  # Returns bogus is no asr occurs.
  noon <- midday(date, location)  # Time when sun nearest zenith.
  phi <- latitude(location)
  delta <- declination(noon, deg(0), solar_longitude(noon))  # Solar declination at noon.
  altitude <- arcsin_degrees(cos_degrees(delta) * cos_degrees(phi) +
                             sin_degrees(delta) * sin_degrees(phi))  # Solar altitude at noon.
  
  if (altitude <= deg(0)) {  # No shadow.
    return(bogus)
  }
  
  # Sun's altitude when shadow increases by its length.
  h <- mod3(arctan_degrees(tan_degrees(altitude), 1 + tan_degrees(altitude)), -90, 90)
  dusk(date, location, -h)
}

# Location of Padua, Italy.
PADUA <- location(angle(45, 24, 28), angle(11, 53, 9), mt(18), hr(1))

local_zero_hour <- function(tee) {
  # TYPE moment -> moment
  # Local time of dusk in Padua, Italy on date of moment tee.
  date <- fixed_from_moment(tee)
  local_from_standard(dusk(date, PADUA, angle(0, 16, 0)) + mn(30), PADUA)
}

italian_from_local <- function(tee_ell) {
  # TYPE moment -> moment
  # Italian time corresponding to local time tee_ell.
  date <- fixed_from_moment(tee_ell)
  z0 <- local_zero_hour(tee_ell - 1)
  z <- local_zero_hour(tee_ell)
  
  if (tee_ell > z) {  # if after zero hour
    tee_ell + (date + 1 - z)  # then next day
  } else {
    tee_ell + (date - z0)
  }
}

local_from_italian <- function(tee) {
  # TYPE moment -> moment
  # Local time corresponding to Italian time tee.
  date <- fixed_from_moment(tee)
  z <- local_zero_hour(tee - 1)
  tee - (date - z)
}

universal_from_dynamical <- function(tee) {
  # TYPE moment -> moment
  # Universal moment from Dynamical time tee.
  tee - ephemeris_correction(tee)
}

dynamical_from_universal <- function(tee_rom_u) {
  # TYPE moment -> moment
  # Dynamical time at Universal moment tee_rom_u.
  tee_rom_u + ephemeris_correction(tee_rom_u)
}

# Noon at start of Gregorian year 2000.
J2000 <- hr(12) + gregorian_new_year(2000)

sidereal_from_moment <- function(tee) {
  # TYPE moment -> angle
  # Mean sidereal time of day from moment tee expressed
  # as hour angle. Adapted from "Astronomical Algorithms"
  # by Jean Meeus, Willmann-Bell, Inc., 2nd edn., 1998, p. 88.
  c <- (tee - J2000) / 36525
  poly(c, deg(c(280.46061837, 36525 * 360.98564736629, 0.000387933, -1/38710000))) %% 360
}

# Mean tropical year duration
MEAN_TROPICAL_YEAR <- 365.242189

# Mean sidereal year duration
MEAN_SIDEREAL_YEAR <- 365.25636

# Mean synodic month duration
MEAN_SYNODIC_MONTH <- 29.530588861

ephemeris_correction <- function(tee) {
  # TYPE moment -> fraction-of-day
  # Dynamical Time minus Universal Time (in days) for
  # moment tee. Adapted from "Astronomical Algorithms"
  # by Jean Meeus, Willmann-Bell (1991) for years
  # 1600-1986 and from polynomials on the NASA
  # Eclipse web site for other years.
  year <- gregorian_year_from_fixed(floor(tee))
  c <- gregorian_date_difference(gregorian_date(1900, january, 1),
                                 gregorian_date(year, july, 1)) / 36525
  
  c2051 <- (-20 + 32 * ((year - 1820) / 100)^2 + 0.5628 * (2150 - year)) / 86400
  y2000 <- year - 2000
  c2006 <- poly(y2000, c(62.92, 0.32217, 0.005589)) / 86400
  c1987 <- poly(y2000, c(63.86, 0.3345, -0.060374, 0.0017275, 0.000651814, 0.00002373599)) / 86400
  c1900 <- poly(c, c(-0.00002, 0.000297, 0.025184, -0.181133, 0.553040, -0.861938, 0.677066, -0.212591))
  c1800 <- poly(c, c(-0.000009, 0.003844, 0.083563, 0.865736, 4.867575, 15.845535, 31.332267, 38.291999, 28.316289, 11.636204, 2.043794))
  
  y1700 <- year - 1700
  c1700 <- poly(y1700, c(8.118780842, -0.005092142, 0.003336121, -0.0000266484)) / 86400
  
  y1600 <- year - 1600
  c1600 <- poly(y1600, c(120, -0.9808, -0.01532, 0.000140272128)) / 86400
  
  y1000 <- (year - 1000) / 100
  c500 <- poly(y1000, c(1574.2, -556.01, 71.23472, 0.319781, -0.8503463, -0.005050998, 0.0083572073)) / 86400
  
  y0 <- year / 100
  c0 <- poly(y0, c(10583.6, -1014.41, 33.78311, -5.952053, -0.1798452, 0.022174192, 0.0090316521)) / 86400
  
  y1820 <- (year - 1820) / 100
  other <- poly(y1820, c(-20, 0, 32)) / 86400
  
  if (year >= 2051 && year <= 2150) {
    c2051
  } else if (year >= 2006 && year <= 2050) {
    c2006
  } else if (year >= 1987 && year <= 2005) {
    c1987
  } else if (year >= 1900 && year <= 1986) {
    c1900
  } else if (year >= 1800 && year <= 1899) {
    c1800
  } else if (year >= 1700 && year <= 1799) {
    c1700
  } else if (year >= 1600 && year <= 1699) {
    c1600
  } else if (year >= 500 && year <= 1599) {
    c500
  } else if (year > -500 && year < 500) {
    c0
  } else {
    other
  }
}

equation_of_time <- function(tee) {
  # TYPE moment -> fraction-of-day
  # Equation of time (as fraction of day) for moment tee.
  # Adapted from "Astronomical Algorithms" by Jean Meeus,
  # Willmann-Bell, 2nd edn., 1998, p. 185.
  c <- julian_centuries(tee)
  lambda <- poly(c, deg(c(280.46645, 36000.76983, 0.0003032)))
  anomaly <- poly(c, deg(c(357.52910, 35999.05030, -0.0001559, -0.00000048)))
  eccentricity <- poly(c, c(0.016708617, -0.000042037, -0.0000001236))
  varepsilon <- obliquity(tee)
  y <- tan_degrees(varepsilon / 2)^2
  
  equation <- (1 / (2 * pi)) * 
    (y * sin_degrees(2 * lambda) - 
     2 * eccentricity * sin_degrees(anomaly) + 
     4 * eccentricity * y * sin_degrees(anomaly) * cos_degrees(2 * lambda) - 
     0.5 * y^2 * sin_degrees(4 * lambda) - 
     1.25 * eccentricity^2 * sin_degrees(2 * anomaly))
  
  sign(equation) * min(abs(equation), hr(12))
}

solar_longitude <- function(tee) {
  # TYPE moment -> season
  # Longitude of sun at moment tee.
  # Adapted from "Planetary Programs and Tables from -4000
  # to +2800" by Pierre Bretagnon and Jean-Louis Simon,
  # Willmann-Bell, 1986.
  c <- julian_centuries(tee)  # moment in Julian centuries
  
  coefficients <- c(403406, 195207, 119433, 112392, 3891, 2819, 1721, 660, 350, 334, 314, 268, 242, 234, 158, 132, 129, 114, 99, 93, 86, 78, 72, 68, 64, 46, 38, 37, 32, 29, 28, 27, 27, 25, 24, 21, 21, 20, 18, 17, 14, 13, 13, 13, 12, 10, 10)
  
  multipliers <- c(0.9287892, 35999.1376958, 35999.4089666, 35998.7287385, 71998.20261, 71998.4403, 36000.35726, 71997.4812, 32964.4678, -19.4410, 445267.1117, 45036.8840, 3.1008, 22518.4434, -19.9739, 65928.9345, 9038.0293, 3034.7684, 33718.148, 3034.448, -2280.773, 29929.992, 31556.493, 149.588, 9037.750, 107997.405, -4444.176, 151.771, 67555.316, 31556.080, -4561.540, 107996.706, 1221.655, 62894.167, 31437.369, 14578.298, -31931.757, 34777.243, 1221.999, 62894.511, -4442.039, 107997.909, 119.066, 16859.071, -4.578, 26895.292, -39.127, 12297.536, 90073.778)
  
  addends <- c(270.54861, 340.19128, 63.91854, 331.26220, 317.843, 86.631, 240.052, 310.26, 247.23, 260.87, 297.82, 343.14, 166.79, 81.53, 3.50, 132.75, 182.95, 162.03, 29.8, 266.4, 249.2, 157.6, 257.8, 185.1, 69.9, 8.0, 197.1, 250.4, 65.3, 162.7, 341.5, 291.6, 98.5, 146.7, 110.0, 5.2, 342.6, 230.9, 256.1, 45.3, 242.9, 115.2, 151.8, 285.3, 53.3, 126.6, 205.7, 85.9, 146.1)
  
  lambda <- deg(282.7771834) + deg(36000.76953744) * c + 
    deg(0.000005729577951308232) * sum(coefficients * sin_degrees(addends + multipliers * c))
  
  (lambda + aberration(tee) + nutation(tee)) %% 360
}

nutation <- function(tee) {
  # TYPE moment -> circle
  # Longitudinal nutation at moment tee.
  c <- julian_centuries(tee)  # moment in Julian centuries
  cap_A <- poly(c, deg(c(124.90, -1934.134, 0.002063)))
  cap_B <- poly(c, deg(c(201.11, 72001.5377, 0.00057)))
  
  deg(-0.004778) * sin_degrees(cap_A) + deg(-0.0003667) * sin_degrees(cap_B)
}

aberration <- function(tee) {
  # TYPE moment -> circle
  # Aberration at moment tee.
  c <- julian_centuries(tee)  # moment in Julian centuries
  deg(0.0000974) * cos_degrees(deg(177.63) + deg(35999.01848) * c) - deg(0.005575)
}

solar_longitude_after <- function(lambda, tee) {
  # TYPE (season moment) -> moment
  # Moment UT of the first time at or after tee
  # when the solar longitude will be lambda degrees.
  rate <- MEAN_TROPICAL_YEAR / deg(360)  # Mean days for 1 degree change.
  tau <- tee + rate * ((lambda - solar_longitude(tee)) %% 360)  # Estimate (within 5 days).
  a <- max(tee, tau - 5)  # At or after tee.
  b <- tau + 5
  
  invert_angular(solar_longitude, lambda, interval_closed(a, b))
}

# Longitude of sun at vernal equinox.
SPRING <- deg(0)

# Longitude of sun at summer solstice.
SUMMER <- deg(90)

# Longitude of sun at autumnal equinox.
AUTUMN <- deg(180)

# Longitude of sun at winter solstice.
WINTER <- deg(270)

season_in_gregorian <- function(season, g_year) {
  # TYPE (season gregorian-year) -> moment
  # Moment UT of season in Gregorian year g_year.
  jan1 <- gregorian_new_year(g_year)
  solar_longitude_after(season, jan1)
}

precession <- function(tee) {
  # TYPE moment -> angle
  # Precession at moment tee using 0,0 as J2000 coordinates.
  # Adapted from "Astronomical Algorithms" by Jean Meeus,
  # Willmann-Bell, 2nd edn., 1998, pp. 136-137.
  c <- julian_centuries(tee)
  eta <- poly(c, c(0, secs(47.0029), secs(-0.03302), secs(0.000060))) %% 360
  cap_P <- poly(c, c(deg(174.876384), secs(-869.8089), secs(0.03536))) %% 360
  p <- poly(c, c(0, secs(5029.0966), secs(1.11113), secs(0.000006))) %% 360
  cap_A <- cos_degrees(eta) * sin_degrees(cap_P)
  cap_B <- cos_degrees(cap_P)
  arg <- arctan_degrees(cap_A, cap_B)
  
  (p + cap_P - arg) %% 360
}

sidereal_solar_longitude <- function(tee) {
  # TYPE moment -> angle
  # Sidereal solar longitude at moment tee
  (solar_longitude(tee) - precession(tee) + SIDEREAL_START) %% 360
}

estimate_prior_solar_longitude <- function(lambda, tee) {
  # TYPE (season moment) -> moment
  # Approximate moment at or before tee
  # when solar longitude just exceeded lambda degrees.
  rate <- MEAN_TROPICAL_YEAR / deg(360)  # Mean change of one degree.
  tau <- tee - rate * ((solar_longitude(tee) - lambda) %% 360)  # First approximation.
  cap_Delta <- mod3(solar_longitude(tau) - lambda, -180, 180)  # Difference in longitude.
  
  min(tee, tau - rate * cap_Delta)
}

mean_lunar_longitude <- function(c) {
  # TYPE century -> angle
  # Mean longitude of moon (in degrees) at moment
  # given in Julian centuries c.
  # Adapted from "Astronomical Algorithms" by Jean Meeus,
  # Willmann-Bell, 2nd edn., 1998, pp. 337-340.
  poly(c, deg(c(218.3164477, 481267.88123421, -0.0015786, 1/538841, -1/65194000))) %% 360
}

lunar_elongation <- function(c) {
  # TYPE century -> angle
  # Elongation of moon (in degrees) at moment
  # given in Julian centuries c.
  # Adapted from "Astronomical Algorithms" by Jean Meeus,
  # Willmann-Bell, 2nd edn., 1998, p. 338.
  poly(c, deg(c(297.8501921, 445267.1114034, -0.0018819, 1/545868, -1/113065000))) %% 360
}

solar_anomaly <- function(c) {
  # TYPE century -> angle
  # Mean anomaly of sun (in degrees) at moment
  # given in Julian centuries c.
  # Adapted from "Astronomical Algorithms" by Jean Meeus,
  # Willmann-Bell, 2nd edn., 1998, p. 338.
  poly(c, deg(c(357.5291092, 35999.0502909, -0.0001536, 1/24490000))) %% 360
}

lunar_anomaly <- function(c) {
  # TYPE century -> angle
  # Mean anomaly of moon (in degrees) at moment
  # given in Julian centuries c.
  # Adapted from "Astronomical Algorithms" by Jean Meeus,
  # Willmann-Bell, 2nd edn., 1998, p. 338.
  poly(c, deg(c(134.9633964, 477198.8675055, 0.0087414, 1/69699, -1/14712000))) %% 360
}

moon_node <- function(c) {
  # TYPE century -> angle
  # Moon's argument of latitude (in degrees) at moment
  # given in Julian centuries c.
  # Adapted from "Astronomical Algorithms" by Jean Meeus,
  # Willmann-Bell, 2nd edn., 1998, p. 338.
  poly(c, deg(c(93.2720950, 483202.0175233, -0.0036539, -1/3526000, 1/863310000))) %% 360
}

lunar_node <- function(date) {
  # TYPE fixed-date -> angle
  # Angular distance of the lunar node from the equinoctial
  # point on fixed date.
  mod3(moon_node(julian_centuries(date)), -90, 90)
}

lunar_longitude <- function(tee) {
  # TYPE moment -> angle
  # Longitude of moon (in degrees) at moment tee.
  # Adapted from "Astronomical Algorithms" by Jean Meeus,
  # Willmann-Bell, 2nd edn., 1998, pp. 338-342.
  c <- julian_centuries(tee)
  cap_L_prime <- mean_lunar_longitude(c)
  cap_D <- lunar_elongation(c)
  cap_M <- solar_anomaly(c)
  cap_M_prime <- lunar_anomaly(c)
  cap_F <- moon_node(c)
  cap_E <- poly(c, c(1, -0.002516, -0.0000074))
  
  args_lunar_elongation <- c(0, 2, 2, 0, 0, 0, 2, 2, 2, 2, 0, 1, 0, 2, 0, 0, 4, 0, 4, 2, 2, 1, 1, 2, 2, 4, 2, 0, 2, 2, 1, 2, 0, 0, 2, 2, 2, 4, 0, 3, 2, 4, 0, 2, 2, 2, 4, 0, 4, 1, 2, 0, 1, 3, 4, 2, 0, 1, 2)
  
  args_solar_anomaly <- c(0, 0, 0, 0, 1, 0, 0, -1, 0, -1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, -1, 0, 0, 0, 1, 0, -1, 0, -2, 1, 2, -2, 0, 0, -1, 0, 0, 1, -1, 2, 2, 1, -1, 0, 0, -1, 0, 1, 0, 1, 0, 0, -1, 2, 1, 0)
  
  args_lunar_anomaly <- c(1, -1, 0, 2, 0, 0, -2, -1, 1, 0, -1, 0, 1, 0, 1, 1, -1, 3, -2, -1, 0, -1, 0, 1, 2, 0, -3, -2, -1, -2, 1, 0, 2, 0, -1, 1, 0, -1, 2, -1, 1, -2, -1, -1, -2, 0, 1, 4, 0, -2, 0, 2, 1, -2, -3, 2, 1, -1, 3)
  
  args_moon_node <- c(0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, -2, 2, -2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, -2, 2, 0, 2, 0, 0, 0, 0, 0, 0, -2, 0, 0, 0, 0, -2, -2, 0, 0, 0, 0, 0, 0, 0)
  
  sine_coeff <- c(6288774, 1274027, 658314, 213618, -185116, -114332, 58793, 57066, 53322, 45758, -40923, -34720, -30383, 15327, -12528, 10980, 10675, 10034, 8548, -7888, -6766, -5163, 4987, 4036, 3994, 3861, 3665, -2689, -2602, 2390, -2348, 2236, -2120, -2069, 2048, -1773, -1595, 1215, -1110, -892, -810, 759, -713, -700, 691, 596, 549, 537, 520, -487, -399, -381, 351, -340, 330, 327, -323, 299, 294)
  
  correction <- deg(1/1000000) * sum(sine_coeff * cap_E^abs(args_solar_anomaly) * 
    sin_degrees(args_lunar_elongation * cap_D + args_solar_anomaly * cap_M + 
                args_lunar_anomaly * cap_M_prime + args_moon_node * cap_F))
  
  venus <- deg(3958/1000000) * sin_degrees(deg(119.75) + c * deg(131.849))
  jupiter <- deg(318/1000000) * sin_degrees(deg(53.09) + c * deg(479264.29))
  flat_earth <- deg(1962/1000000) * sin_degrees(cap_L_prime - cap_F)
  
  (cap_L_prime + correction + venus + jupiter + flat_earth + nutation(tee)) %% 360
}

sidereal_lunar_longitude <- function(tee) {
  # TYPE moment -> angle
  # Sidereal lunar longitude at moment tee.
  (lunar_longitude(tee) - precession(tee) + SIDEREAL_START) %% 360
}

nth_new_moon <- function(n) {
  # TYPE integer -> moment
  # Moment of n-th new moon after (or before) the new moon
  # of January 11, 1. Adapted from "Astronomical Algorithms"
  # by Jean Meeus, Willmann-Bell, corrected 2nd edn., 2005.
  n0 <- 24724  # Months from RD 0 until j2000.
  k <- n - n0  # Months since j2000.
  c <- k / 1236.85  # Julian centuries.
  
  approx <- J2000 + poly(c, c(5.09766, MEAN_SYNODIC_MONTH * 1236.85, 0.00015437, -0.000000150, 0.00000000073))
  
  cap_E <- poly(c, c(1, -0.002516, -0.0000074))
  solar_anomaly <- poly(c, deg(c(2.5534, 1236.85 * 29.10535670, -0.0000014, -0.00000011)))
  lunar_anomaly <- poly(c, deg(c(201.5643, 385.81693528 * 1236.85, 0.0107582, 0.00001238, -0.000000058)))
  moon_argument <- poly(c, deg(c(160.7108, 390.67050284 * 1236.85, -0.0016118, -0.00000227, 0.000000011)))
  cap_omega <- poly(c, deg(c(124.7746, -1.56375588 * 1236.85, 0.0020672, 0.00000215)))
  
  E_factor <- c(0, 1, 0, 0, 1, 1, 2, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  solar_coeff <- c(0, 1, 0, 0, -1, 1, 2, 0, 0, 1, 0, 1, 1, -1, 2, 0, 3, 1, 0, 1, -1, -1, 1, 0)
  lunar_coeff <- c(1, 0, 2, 0, 1, 1, 0, 1, 1, 2, 3, 0, 0, 2, 1, 2, 0, 1, 2, 1, 1, 1, 3, 4)
  moon_coeff <- c(0, 0, 0, 2, 0, 0, 0, -2, 2, 0, 0, 2, -2, 0, 0, -2, 0, -2, 2, 2, 2, -2, 0, 0)
  sine_coeff <- c(-0.40720, 0.17241, 0.01608, 0.01039, 0.00739, -0.00514, 0.00208, -0.00111, -0.00057, 0.00056, -0.00042, 0.00042, 0.00038, -0.00024, -0.00007, 0.00004, 0.00004, 0.00003, 0.00003, -0.00003, 0.00003, -0.00002, -0.00002, 0.00002)
  
  correction <- -0.00017 * sin_degrees(cap_omega) + 
    sum(sine_coeff * cap_E^E_factor * 
        sin_degrees(solar_coeff * solar_anomaly + lunar_coeff * lunar_anomaly + moon_coeff * moon_argument))
  
  add_const <- c(251.88, 251.83, 349.42, 84.66, 141.74, 207.14, 154.84, 34.52, 207.19, 291.34, 161.72, 239.56, 331.55)
  add_coeff <- c(0.016321, 26.651886, 36.412478, 18.206239, 53.303771, 2.453732, 7.306860, 27.261239, 0.121824, 1.844379, 24.198154, 25.513099, 3.592518)
  add_factor <- c(0.000165, 0.000164, 0.000126, 0.000110, 0.000062, 0.000060, 0.000056, 0.000047, 0.000042, 0.000040, 0.000037, 0.000035, 0.000023)
  
  extra <- 0.000325 * sin_degrees(poly(c, deg(c(299.77, 132.8475848, -0.009173))))
  additional <- sum(add_factor * sin_degrees(add_const + add_coeff * k))
  
  universal_from_dynamical(approx + correction + extra + additional)
}

new_moon_before <- function(tee) {
  # TYPE moment -> moment
  # Moment UT of last new moon before tee.
  t0 <- nth_new_moon(0)
  phi <- lunar_phase(tee)
  n <- round((tee - t0) / MEAN_SYNODIC_MONTH - phi / deg(360))
  
  nth_new_moon(final_func(function(k) nth_new_moon(k) < tee, n - 1))
}

new_moon_at_or_after <- function(tee) {
  # TYPE moment -> moment
  # Moment UT of first new moon at or after tee.
  t0 <- nth_new_moon(0)
  phi <- lunar_phase(tee)
  n <- round((tee - t0) / MEAN_SYNODIC_MONTH - phi / deg(360))
  
  nth_new_moon(next_func(function(k) nth_new_moon(k) >= tee, n))
}

lunar_phase <- function(tee) {
  # TYPE moment -> phase
  # Lunar phase, as an angle in degrees, at moment tee.
  # An angle of 0 means a new moon, 90 degrees means the
  # first quarter, 180 means a full moon, and 270 degrees
  # means the last quarter.
  phi <- (lunar_longitude(tee) - solar_longitude(tee)) %% 360
  t0 <- nth_new_moon(0)
  n <- round((tee - t0) / MEAN_SYNODIC_MONTH)
  phi_prime <- deg(360) * ((tee - nth_new_moon(n)) / MEAN_SYNODIC_MONTH %% 1)
  
  if (abs(phi - phi_prime) > deg(180)) {  # close call
    phi_prime
  } else {
    phi
  }
}

lunar_phase_at_or_before <- function(phi, tee) {
  # TYPE (phase moment) -> moment
  # Moment UT of the last time at or before tee
  # when the lunar-phase was phi degrees.
  tau <- tee - MEAN_SYNODIC_MONTH * (1 / deg(360)) * ((lunar_phase(tee) - phi) %% 360)  # Estimate.
  a <- tau - 2
  b <- min(tee, tau + 2)  # At or before tee.
  
  invert_angular(lunar_phase, phi, interval_closed(a, b))
}

# Excess of lunar longitude over solar longitude at new moon.
NEW <- deg(0)

# Excess of lunar longitude over solar longitude at first quarter moon.
FIRST_QUARTER <- deg(90)

# Excess of lunar longitude over solar longitude at full moon.
FULL <- deg(180)

# Excess of lunar longitude over solar longitude at last quarter moon.
LAST_QUARTER <- deg(270)

lunar_phase_at_or_after <- function(phi, tee) {
  # TYPE (phase moment) -> moment
  # Moment UT of the next time at or after tee
  # when the lunar-phase is phi degrees.
  tau <- tee + MEAN_SYNODIC_MONTH * (1 / deg(360)) * ((phi - lunar_phase(tee)) %% 360)  # Estimate.
  a <- max(tee, tau - 2)  # At or after tee.
  b <- tau + 2
  
  invert_angular(lunar_phase, phi, interval_closed(a, b))
}

lunar_latitude <- function(tee) {
  # TYPE moment -> half-circle
  # Latitude of moon (in degrees) at moment tee.
  # Adapted from "Astronomical Algorithms" by Jean Meeus,
  # Willmann-Bell, 2nd edn., 1998, pp. 338-342.
  c <- julian_centuries(tee)
  cap_L_prime <- mean_lunar_longitude(c)
  cap_D <- lunar_elongation(c)
  cap_M <- solar_anomaly(c)
  cap_M_prime <- lunar_anomaly(c)
  cap_F <- moon_node(c)
  cap_E <- poly(c, c(1, -0.002516, -0.0000074))
  
  args_lunar_elongation <- c(0, 0, 0, 2, 2, 2, 2, 0, 2, 0, 2, 2, 2, 2, 2, 2, 2, 0, 4, 0, 0, 0, 1, 0, 0, 0, 1, 0, 4, 4, 0, 4, 2, 2, 2, 2, 0, 2, 2, 2, 2, 4, 2, 2, 0, 2, 1, 1, 0, 2, 1, 2, 0, 4, 4, 1, 4, 1, 4, 2)
  
  args_solar_anomaly <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 1, -1, -1, -1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1, 1, 0, -1, -2, 0, 1, 1, 1, 1, 1, 0, -1, 1, 0, -1, 0, 0, 0, -1, -2)
  
  args_lunar_anomaly <- c(0, 1, 1, 0, -1, -1, 0, 2, 1, 2, 0, -2, 1, 0, -1, 0, -1, -1, -1, 0, 0, -1, 0, 1, 1, 0, 0, 3, 0, -1, 1, -2, 0, 2, 1, -2, 3, 2, -3, -1, 0, 0, 1, 0, 1, 1, 0, 0, -2, -1, 1, -2, 2, -2, -1, 1, 1, -1, 0, 0)
  
  args_moon_node <- c(1, 1, -1, -1, 1, -1, 1, 1, -1, -1, -1, -1, 1, -1, 1, 1, -1, -1, -1, 1, 3, 1, 1, 1, -1, -1, -1, 1, -1, 1, -3, 1, -3, -1, -1, 1, -1, 1, 1, 1, 1, -1, 3, -1, -1, 1, -1, -1, 1, -1, 1, -1, -1, -1, -1, -1, -1, 1)
  
  sine_coeff <- c(5128122, 280602, 277693, 173237, 55413, 46271, 32573, 17198, 9266, 8822, 8216, 4324, 4200, -3359, 2463, 2211, 2065, -1870, 1828, -1794, -1749, -1565, -1491, -1475, -1410, -1344, -1335, 1107, 1021, 833, 777, 671, 607, 596, 491, -451, 439, 422, 421, -366, -351, 331, 315, 302, -283, -229, 223, 223, -220, -220, -185, 181, -177, 176, 166, -164, 132, -119, 115, 107)
  
  beta <- deg(1/1000000) * sum(sine_coeff * cap_E^abs(args_solar_anomaly) * 
    sin_degrees(args_lunar_elongation * cap_D + args_solar_anomaly * cap_M + 
                args_lunar_anomaly * cap_M_prime + args_moon_node * cap_F))
  
  venus <- deg(175/1000000) * 
    (sin_degrees(deg(119.75) + c * deg(131.849) + cap_F) + 
     sin_degrees(deg(119.75) + c * deg(131.849) - cap_F))
  
  flat_earth <- deg(-2235/1000000) * sin_degrees(cap_L_prime) + 
    deg(127/1000000) * sin_degrees(cap_L_prime - cap_M_prime) + 
    deg(-115/1000000) * sin_degrees(cap_L_prime + cap_M_prime)
  
  extra <- deg(382/1000000) * sin_degrees(deg(313.45) + c * deg(481266.484))
  
  beta + venus + flat_earth + extra
}

lunar_altitude <- function(tee, location) {
  # TYPE (moment location) -> half-circle
  # Geocentric altitude of moon at tee at location, 
  # as a small positive/negative angle in degrees, ignoring
  # parallax and refraction. Adapted from "Astronomical
  # Algorithms" by Jean Meeus, Willmann-Bell, 2nd edn., 1998.
  phi <- latitude(location)  # Local latitude.
  psi <- longitude(location)  # Local longitude.
  lambda <- lunar_longitude(tee)  # Lunar longitude.
  beta <- lunar_latitude(tee)  # Lunar latitude.
  alpha <- right_ascension(tee, beta, lambda)  # Lunar right ascension.
  delta <- declination(tee, beta, lambda)  # Lunar declination.
  theta0 <- sidereal_from_moment(tee)  # Sidereal time.
  cap_H <- (theta0 - psi - alpha) %% 360  # Local hour angle.
  
  altitude <- arcsin_degrees(sin_degrees(phi) * sin_degrees(delta) + 
                             cos_degrees(phi) * cos_degrees(delta) * cos_degrees(cap_H))
  
  mod3(altitude, -180, 180)
}

lunar_distance <- function(tee) {
  # TYPE moment -> distance
  # Distance to moon (in meters) at moment tee.
  # Adapted from "Astronomical Algorithms" by Jean Meeus,
  # Willmann-Bell, 2nd edn., 1998, pp. 338-342.
  c <- julian_centuries(tee)
  cap_D <- lunar_elongation(c)
  cap_M <- solar_anomaly(c)
  cap_M_prime <- lunar_anomaly(c)
  cap_F <- moon_node(c)
  cap_E <- poly(c, c(1, -0.002516, -0.0000074))
  
  args_lunar_elongation <- c(0, 2, 2, 0, 0, 0, 2, 2, 2, 2, 0, 1, 0, 2, 0, 0, 4, 0, 4, 2, 2, 1, 1, 2, 2, 4, 2, 0, 2, 2, 1, 2, 0, 0, 2, 2, 2, 4, 0, 3, 2, 4, 0, 2, 2, 2, 4, 0, 4, 1, 2, 0, 1, 3, 4, 2, 0, 1, 2, 2)
  
  args_solar_anomaly <- c(0, 0, 0, 0, 1, 0, 0, -1, 0, -1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, -1, 0, 0, 0, 1, 0, -1, 0, -2, 1, 2, -2, 0, 0, -1, 0, 0, 1, -1, 2, 2, 1, -1, 0, 0, -1, 0, 1, 0, 1, 0, 0, -1, 2, 1, 0, 0)
  
  args_lunar_anomaly <- c(1, -1, 0, 2, 0, 0, -2, -1, 1, 0, -1, 0, 1, 0, 1, 1, -1, 3, -2, -1, 0, -1, 0, 1, 2, 0, -3, -2, -1, -2, 1, 0, 2, 0, -1, 1, 0, -1, 2, -1, 1, -2, -1, -1, -2, 0, 1, 4, 0, -2, 0, 2, 1, -2, -3, 2, 1, -1, 3, -1)
  
  args_moon_node <- c(0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, -2, 2, -2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, -2, 2, 0, 2, 0, 0, 0, 0, 0, 0, -2, 0, 0, 0, 0, -2, -2, 0, 0, 0, 0, 0, 0, 0, -2)
  
  cosine_coeff <- c(-20905355, -3699111, -2955968, -569925, 48888, -3149, 246158, -152138, -170733, -204586, -129620, 108743, 104755, 10321, 0, 79661, -34782, -23210, -21636, 24208, 30824, -8379, -16675, -12831, -10445, -11650, 14403, -7003, 0, 10056, 6322, -9884, 5751, 0, -4950, 4130, 0, -3958, 0, 3258, 2616, -1897, -2117, 2354, 0, 0, -1423, -1117, -1571, -1739, 0, -4421, 0, 0, 0, 0, 1165, 0, 0, 8752)
  
  correction <- sum(cosine_coeff * cap_E^abs(args_solar_anomaly) * 
    cos_degrees(args_lunar_elongation * cap_D + args_solar_anomaly * cap_M + 
                args_lunar_anomaly * cap_M_prime + args_moon_node * cap_F))
  
  mt(385000560) + correction
}

lunar_parallax <- function(tee, location) {
  # TYPE (moment location) -> angle
  # Parallax of moon at tee at location.
  # Adapted from "Astronomical Algorithms" by Jean Meeus,
  # Willmann-Bell, 2nd edn., 1998.
  geo <- lunar_altitude(tee, location)
  cap_Delta <- lunar_distance(tee)
  alt <- mt(6378140) / cap_Delta
  arg <- alt * cos_degrees(geo)
  
  arcsin_degrees(arg)
}

topocentric_lunar_altitude <- function(tee, location) {
  # TYPE (moment location) -> half-circle
  # Topocentric altitude of moon at tee at location, 
  # as a small positive/negative angle in degrees,
  # ignoring refraction.
  lunar_altitude(tee, location) - lunar_parallax(tee, location)
}

lunar_diameter <- function(tee) {
  # TYPE moment -> angle
  # Geocentric apparent lunar diameter of the moon (in
  # degrees) at moment tee. Adapted from "Astronomical
  # Algorithms" by Jean Meeus, Willmann-Bell, 2nd edn., 1998.
  deg(1792367000/9) / lunar_distance(tee)
}

observed_lunar_altitude <- function(tee, location) {
  # TYPE (moment location) -> half-circle
  # Observed altitude of upper limb of moon at tee at location, 
  # as a small positive/negative angle in degrees, including
  # refraction and elevation.
  topocentric_lunar_altitude(tee, location) + refraction(tee, location) + mins(16)
}

moonset <- function(date, location) {
  # TYPE (fixed-date location) -> moment
  # Standard time of moonset on fixed date at location.
  # Returns bogus if there is no moonset on date.
  tee <- universal_from_standard(date, location)  # Midnight.
  waxing <- lunar_phase(tee) < deg(180)
  alt <- observed_lunar_altitude(tee, location)  # Altitude at midnight.
  lat <- latitude(location)
  offset <- alt / (4 * (deg(90) - abs(lat)))
  
  if (waxing) {
    approx <- if (offset > 0) (tee + offset) else (tee + 1 + offset)
  } else {
    approx <- tee - offset + 1/2
  }
  
  set <- binary_search(
    function(x) observed_lunar_altitude(x, location) < deg(0),
    approx - hr(6), approx + hr(6), mn(1))
  
  if (set < (date + 1)) {
    max(standard_from_universal(set, location), date)  # May be just before midnight.
  } else {
    bogus  # Else no moonset this day.
  }
}

moonrise <- function(date, location) {
  # TYPE (fixed-date location) -> moment
  # Standard time of moonrise on fixed date at location.
  # Returns bogus if there is no moonrise on date.
  tee <- universal_from_standard(date, location)  # Midnight.
  waning <- lunar_phase(tee) > deg(180)
  alt <- observed_lunar_altitude(tee, location)  # Altitude at midnight.
  lat <- latitude(location)
  offset <- alt / (4 * (deg(90) - abs(lat)))
  
  if (waning) {
    approx <- if (offset > 0) (tee - 1 + offset) else (tee - offset)
  } else {
    approx <- tee + 1/2 + offset
  }
  
  rise <- binary_search(
    function(x) observed_lunar_altitude(x, location) > deg(0),
    approx - hr(6), approx + hr(6), mn(1))
  
  if (rise < (date + 1)) {
    max(standard_from_universal(rise, location), date)  # May be just before midnight.
  } else {
    bogus  # Else no moonrise this day.
  }
}
