# ==============================================================================
# Section: Time and Astronomy
# ==============================================================================

#' Sunrise and sunset given a date and location
#'
#' Calculate the time of sunrise and sunset at a specific location and date. The
#' time zone of the location is used as specified in the `location` object.
#'
#' @param date Date in rd_fixed format
#' @param location Location of class "location", usually the output from the `location` function
#' @param ... Additional arguments passed to specific methods
#' @return Time of sunrise
#' @examples
#' melbourne <- location(-37.8136, 144.9631, 31, 10)
#' sydney <- location(-33.8688, 151.2093, 3, 10)
#' sunrise("2025-01-01", c(melbourne, sydney))
#' sunset("2025-01-01", c(melbourne, sydney))
#' @export
sunrise <- function(date, location, ...) {
  UseMethod("sunrise")
}

#' @export
#' @rdname sunrise
sunset <- function(date, location, ...) {
  UseMethod("sunset")
}

#' @export
sunrise.rd_fixed <- function(date, location, as_time = TRUE, ...) {
  lst <- vec_recycle_common(
    date = date,
    location = location
  )
  alpha <- refraction(lst$date, lst$location)
  output <- dawn(lst$date, lst$location, alpha)
  if (as_time) {
    as_time_of_day(output)
  } else {
    output
  }
}

#' @export
sunrise.default <- function(date, location, ...) {
  sunrise(as_rd(date), location, ...)
}

#' @export
sunset.rd_fixed <- function(date, location, as_time = TRUE, ...) {
  lst <- vec_recycle_common(
    date = date,
    location = location
  )
  alpha <- refraction(lst$date, lst$location)
  output <- dusk(lst$date, lst$location, alpha)
  if (as_time) {
    as_time_of_day(output)
  } else {
    output
  }
}

#' @export
sunset.default <- function(date, location, ...) {
  sunset(as_rd(date), location, ...)
}


refraction <- function(tee, location) {
  # Return refraction angle at location 'location' and time 'tee'
  h <- pmax(mt(0), elevation(location))
  cap_R <- mt(6.372E6)
  dip <- arccos_degrees(cap_R / (cap_R + h))
  return(angle(0, 50, 0) + dip + 19 / 3600 * sqrt(h))
}

hr <- function(x) {
  # x hours
  x / 24
}

mt <- function(x) {
  # x meters
  x
}

deg <- function(x) {
  # x degrees
  x
}

angle <- function(d, m, s) {
  # d degrees, m arcminutes, s arcseconds
  d + (m + s / 60) / 60
}

degrees <- function(theta) {
  # Normalize angle theta to range 0-360 degrees
  theta %% 360
}

radians_to_degrees <- function(theta) {
  # Convert angle theta from radians to degrees
  degrees(theta * 180 / pi)
}

degrees_to_radians <- function(theta) {
  # Convert angle theta from degrees to radians
  degrees(theta) * pi / 180
}

sin_degrees <- function(theta) {
  # Sine of theta (given in degrees)
  sin(degrees_to_radians(theta))
}

cosine_degrees <- function(theta) {
  # Cosine of theta (given in degrees)
  cos(degrees_to_radians(theta))
}

tangent_degrees <- function(theta) {
  # Tangent of theta (given in degrees)
  tan(degrees_to_radians(theta))
}

arctan_degrees <- function(x, quad) {
  # Arctangent of x in degrees in quadrant quad
  alpha <- radians_to_degrees(atan(x))
  alpha[!quad %in% c(1, 4)] <- alpha[!quad %in% c(1, 4)] + 180
  alpha %% 360
}

arcsin_degrees <- function(x) {
  # Arcsine of x in degrees
  radians_to_degrees(asin(x))
}

arccos_degrees <- function(x) {
  # Arccosine of x in degrees
  radians_to_degrees(acos(x))
}


julian_centuries <- function(tee) {
  # Julian centuries since 2000 at moment tee
  (dynamical_from_universal(tee) - J2000) / 36525
}

obliquity <- function(tee) {
  # Obliquity of ecliptic at moment tee
  angle(23, 26, 21.448) +
    poly(
      julian_centuries(tee),
      c(0, angle(0, 0, -46.8150), angle(0, 0, -0.00059), angle(0, 0, 0.001813))
    )
}

precise_obliquity <- function(tee) {
  # Return precise (mean) obliquity of ecliptic at moment tee
  u <- julian_centuries(tee) / 100
  return(poly(u, c(
    angle(23, 26, 21.448),
    angle(0, 0, -4680.93),
    angle(0, 0, -1.55),
    angle(0, 0, 1999.25),
    angle(0, 0, -51.38),
    angle(0, 0, -249.67),
    angle(0, 0, -39.05),
    angle(0, 0, 7.12),
    angle(0, 0, 27.87),
    angle(0, 0, 5.79),
    angle(0, 0, 2.45)
  )))
}

sine_offset <- function(tee, location, alpha) {
  # Return sine of angle between position of sun at
  # local time tee and when its depression is alpha at location, location.
  # Out of range when it does not occur
  phi <- latitude(location)
  tee_prime <- universal_from_local(tee, location)
  delta <- declination(tee_prime, deg(0), solar_longitude(tee_prime))
  return((tangent_degrees(phi) * tangent_degrees(delta)) +
    (sin_degrees(alpha) / (cosine_degrees(delta) *
      cosine_degrees(phi))))
}

moment_from_depression <- function(approx, locale, alpha) {
  # Moment in Local Time near approx when depression angle of sun is alpha
  # (negative if above horizon) at locale; bogus if never occurs
  approx <- vec_data(approx)
  phi <- latitude(locale)
  tee <- universal_from_local(approx, locale)
  # Declination of sun
  delta <- arcsin_degrees(
    sin_degrees(obliquity(tee)) * sin_degrees(solar_longitude(tee))
  )
  morning <- (approx %% 1) < 0.5
  sine_offset <- tangent_degrees(phi) *
    tangent_degrees(delta) +
    sin_degrees(alpha) / (cosine_degrees(delta) * cosine_degrees(phi))
  result <- local_from_apparent(
    floor(approx) +
      0.5 +
      (1 - 2 * morning) *
        ((0.5 + arcsin_degrees(sine_offset) / 360) %% 1 - 0.25),
    locale
  )
  result[abs(sine_offset) > 1] <- NA_real_
  return(result)
}

dawn <- function(date, locale, alpha) {
  # Standard time in morning of date at locale when depression angle of sun is alpha
  # First, get an approximate time
  approx <- moment_from_depression(date + 0.25, locale, alpha)
  # Then refine the calculation
  date[!is.na(approx)] <- approx[!is.na(approx)]
  result <- moment_from_depression(date, locale, alpha)
  standard_from_local(result, locale)
}

dusk <- function(date, locale, alpha) {
  # Standard time in evening on date at locale when depression angle of sun is alpha
  # First, get an approximate time
  approx <- moment_from_depression(date + 0.75, locale, alpha)
  # Then refine the calculation
  approx[!is.na(approx)] <- date[!is.na(approx)] + 0.99
  result <- moment_from_depression(approx, locale, alpha)
  standard_from_local(result, locale)
}

asr <- function(date, locale, ...) {
  # Standard time of asr on fixed date at locale
  # Time when sun nearest zenith
  noon <- universal_from_standard(midday(date, locale), locale)
  phi <- latitude(locale)

  # Solar declination at noon
  delta <- arcsin_degrees(
    sin_degrees(obliquity(noon)) * sin_degrees(solar_longitude(noon))
  )

  # Solar altitude at noon
  altitude <- arcsin_degrees(
    sin_degrees(phi) *
      sin_degrees(delta) +
      cosine_degrees(phi) * cosine_degrees(delta)
  )

  # Sun's altitude when shadow increases by double its length
  h <- arctan_degrees(
    tangent_degrees(altitude) / (1 + 2 * tangent_degrees(altitude)),
    1
  )
  dusk(date, locale, -h)
}


temporal_hour <- function(date, locale) {
  # Length of daytime temporal hour on fixed date at locale
  (sunset(date, locale, as_time = FALSE) -
    sunrise(date, locale, as_time = FALSE)) /
    12
}

standard_from_sundial <- function(date, hour, locale) {
  # Standard time on fixed date of temporal hour at locale
  tee <- temporal_hour(date, locale)
  result <- sunrise(date, locale, as_time = FALSE)
  daylight <- 6 <= hour & hour <= 18
  result[daylight] <- result[daylight] + (hour[daylight] - 6) * tee[daylight]
  result[!daylight] <- result[!daylight] +
    (hour[!daylight] - 6) * (1 / 12 - tee[!daylight])
  result
}

universal_from_dynamical <- function(tee) {
  # Universal moment from Dynamical time tee
  tee - ephemeris_correction(tee)
}

dynamical_from_universal <- function(tee) {
  # Dynamical time at Universal moment tee
  tee + ephemeris_correction(tee)
}


sidereal_from_moment <- function(tee) {
  # Mean sidereal time of day from moment tee expressed as hour angle
  # Adapted from "Astronomical Algorithms" by Jean Meeus, Willmann-Bell, Inc., 1991
  u <- (tee - J2000) / 36525

  deg(poly(
    u,
    c(280.46061837, 36525 * 360.98564736629, 0.000387933, -1 / 38710000)
  )) %%
    360
}

ephemeris_correction <- function(tee) {
  # Dynamical Time minus Universal Time (in days) for fixed time tee
  # Adapted from "Astronomical Algorithms" by Jean Meeus, Willmann-Bell, Inc., 1991
  year <- gregorian_year_from_fixed(floor(tee))

  u <- (as_rd(gregorian(year, JULY, 1)) - as_rd(gregorian(1900, JANUARY, 1))) /
    36525

  result1 <- (year - 1933) / (24 * 60 * 60)
  result2 <- poly(
    u,
    c(
      -0.00002, 0.000297, 0.025184, -0.181133, 0.553040, -0.861938, 0.677066,
      -0.212591
    )
  )
  result3 <- poly(
    u,
    c(
      -0.000009, 0.003844, 0.083563, 0.865736, 4.867575, 15.845535, 31.332267,
      38.291999, 28.316289, 11.636204, 2.043794
    )
  )
  result4 <- poly(year - 1600, c(196.58333, -4.0675, 0.0219167)) /
    (24 * 60 * 60)
  x <- hr(12) +
    as_rd(gregorian(year, JANUARY, 1)) -
    as_rd(gregorian(1810, JANUARY, 1))
  result <- (x * x / 41048480 - 15) / (24 * 60 * 60)

  result[1988 <= year & year <= 2019] <- result1[1988 <= year & year <= 2019]
  result[1900 <= year & year <= 1987] <- result2[1900 <= year & year <= 1987]
  result[1800 <= year & year <= 1899] <- result3[1800 <= year & year <= 1899]
  result[1620 <= year & year <= 1799] <- result4[1620 <= year & year <= 1799]
  return(result)
}

equation_of_time <- function(tee) {
  # Equation of time (as fraction of day) for moment tee
  # Adapted from "Astronomical Algorithms" by Jean Meeus, Willmann-Bell, Inc., 1991
  u <- julian_centuries(tee)

  longitude <- poly(u, deg(c(280.46645, 36000.76983, 0.0003032)))

  anomaly <- poly(u, deg(c(357.52910, 35999.05030, -0.0001559, -0.00000048)))

  eccentricity <- poly(u, deg(c(0.016708617, -0.000042037, -0.0000001236)))

  varepsilon <- obliquity(tee)

  y <- tangent_degrees(varepsilon / 2)^2

  equation <- 1 /
    (2 * pi) *
    (y *
      sin_degrees(2 * longitude) +
      -2 * eccentricity * sin_degrees(anomaly) +
      4 *
        eccentricity *
        y *
        sin_degrees(anomaly) *
        cosine_degrees(2 * longitude) +
      -0.5 * y * y * sin_degrees(4 * longitude) +
      -1.25 * eccentricity * eccentricity * sin_degrees(2 * anomaly))

  sign(equation) * pmin(abs(equation), hr(12))
}

solar_longitude <- function(tee) {
  # Longitude of sun at moment tee
  # Adapted from "Planetary Programs and Tables from -4000 to +2800"
  # by Pierre Bretagnon and Jean-Louis Simon, Willmann-Bell, Inc., 1986
  u <- julian_centuries(tee)

  coefficients <- c(
    403406, 195207, 119433, 112392, 3891, 2819, 1721,
    660, 350, 334, 314, 268, 242, 234, 158, 132, 129, 114,
    99, 93, 86, 78, 72, 68, 64, 46, 38, 37, 32, 29, 28, 27, 27,
    25, 24, 21, 21, 20, 18, 17, 14, 13, 13, 13, 12, 10, 10, 10, 10
  )

  multipliers <- c(
    0.9287892, 35999.1376958, 35999.4089666,
    35998.7287385, 71998.20261, 71998.4403,
    36000.35726, 71997.4812, 32964.4678,
    -19.4410, 445267.1117, 45036.8840, 3.1008,
    22518.4434, -19.9739, 65928.9345,
    9038.0293, 3034.7684, 33718.148, 3034.448,
    -2280.773, 29929.992, 31556.493, 149.588,
    9037.750, 107997.405, -4444.176, 151.771,
    67555.316, 31556.080, -4561.540,
    107996.706, 1221.655, 62894.167,
    31437.369, 14578.298, -31931.757,
    34777.243, 1221.999, 62894.511,
    -4442.039, 107997.909, 119.066, 16859.071,
    -4.578, 26895.292, -39.127, 12297.536,
    90073.778
  )

  addends <- c(
    270.54861, 340.19128, 63.91854, 331.26220,
    317.843, 86.631, 240.052, 310.26, 247.23,
    260.87, 297.82, 343.14, 166.79, 81.53,
    3.50, 132.75, 182.95, 162.03, 29.8,
    266.4, 249.2, 157.6, 257.8, 185.1, 69.9,
    8.0, 197.1, 250.4, 65.3, 162.7, 341.5,
    291.6, 98.5, 146.7, 110.0, 5.2, 342.6,
    230.9, 256.1, 45.3, 242.9, 115.2, 151.8,
    285.3, 53.3, 126.6, 205.7, 85.9,
    146.1
  )

  # Calculate the sum part
  parts <- matrix(0, nrow = length(coefficients), ncol = length(u))
  for (i in seq_along(coefficients)) {
    parts[i, ] <- coefficients[i] *
      sin_degrees(addends[i] + multipliers[i] * u)
  }

  longitude <- deg(282.7771834) + 36000.76953744 * u +
    0.000005729577951308232 * colSums(parts)

  (longitude + aberration(tee) + nutation(tee)) %% 360
}

nutation <- function(tee) {
  # Longitudinal nutation at moment tee
  u <- julian_centuries(tee)

  cap_A <- poly(u, deg(c(124.90, -1934.134, 0.002063)))
  cap_B <- poly(u, deg(c(201.11, 72001.5377, 0.00057)))

  deg(-0.004778) * sin_degrees(cap_A) + deg(-0.0003667) * sin_degrees(cap_B)
}

aberration <- function(tee) {
  # Aberration at moment tee
  u <- julian_centuries(tee)

  deg(0.0000974) *
    cosine_degrees(deg(177.63) + deg(35999.01848) * u) -
    deg(0.005575)
}

# Lunar position functions
mean_lunar_longitude <- function(c) {
  # Return mean longitude of moon (in degrees) at moment
  # given in Julian centuries c (including the constant term of the
  # effect of the light-time (-0".70).
  # Adapted from eq. 47.1 in "Astronomical Algorithms" by Jean Meeus,
  # Willmann_Bell, Inc., 2nd ed. with corrections, 2005
  degrees(poly(c, deg(c(
    218.3164477, 481267.88123421,
    -0.0015786, 1 / 538841,
    -1 / 65194000
  ))))
}

lunar_elongation <- function(c) {
  # Return elongation of moon (in degrees) at moment
  # given in Julian centuries c.
  # Adapted from eq. 47.2 in "Astronomical Algorithms" by Jean Meeus,
  # Willmann_Bell, Inc., 2nd ed. with corrections, 2005
  degrees(poly(c, deg(c(
    297.8501921, 445267.1114034,
    -0.0018819, 1 / 545868,
    -1 / 113065000
  ))))
}

solar_anomaly <- function(c) {
  # Return mean anomaly of sun (in degrees) at moment
  # given in Julian centuries c.
  # Adapted from eq. 47.3 in "Astronomical Algorithms" by Jean Meeus,
  # Willmann_Bell, Inc., 2nd ed. with corrections, 2005
  degrees(poly(c, deg(c(
    357.5291092, 35999.0502909,
    -0.0001536, 1 / 24490000
  ))))
}

lunar_anomaly <- function(c) {
  # Return mean anomaly of moon (in degrees) at moment
  # given in Julian centuries c.
  # Adapted from eq. 47.4 in "Astronomical Algorithms" by Jean Meeus,
  # Willmann_Bell, Inc., 2nd ed. with corrections, 2005
  degrees(poly(c, deg(c(
    134.9633964, 477198.8675055,
    0.0087414, 1 / 69699,
    -1 / 14712000
  ))))
}

moon_node <- function(c) {
  # Return Moon's argument of latitude (in degrees) at moment
  # given in Julian centuries 'c'.
  # Adapted from eq. 47.5 in "Astronomical Algorithms" by Jean Meeus,
  # Willmann_Bell, Inc., 2nd ed. with corrections, 2005
  degrees(poly(c, deg(c(
    93.2720950, 483202.0175233,
    -0.0036539, -1 / 3526000,
    1 / 863310000
  ))))
}

lunar_longitude <- function(tee) {
  # Longitude of moon (in degrees) at moment tee
  # Adapted from "Astronomical Algorithms" by Jean Meeus, Willmann-Bell, Inc., 1991
  u <- julian_centuries(tee)

  cap_L_prime <- mean_lunar_longitude(u)
  cap_D <- lunar_elongation(u)
  cap_M <- solar_anomaly(u)
  cap_M_prime <- lunar_anomaly(u)
  cap_F <- moon_node(u)
  # see eq. 47.6 in Meeus
  cap_E <- poly(u, c(1, -0.002516, -0.0000074))

  args_lunar_elongation <- c(
    0, 2, 2, 0, 0, 0, 2, 2, 2, 2, 0, 1, 0, 2, 0, 0, 4, 0, 4, 2, 2, 1,
    1, 2, 2, 4, 2, 0, 2, 2, 1, 2, 0, 0, 2, 2, 2, 4, 0, 3, 2, 4, 0, 2,
    2, 2, 4, 0, 4, 1, 2, 0, 1, 3, 4, 2, 0, 1, 2
  )

  args_solar_anomaly <- c(
    0, 0, 0, 0, 1, 0, 0, -1, 0, -1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1,
    0, 1, -1, 0, 0, 0, 1, 0, -1, 0, -2, 1, 2, -2, 0, 0, -1, 0, 0, 1,
    -1, 2, 2, 1, -1, 0, 0, -1, 0, 1, 0, 1, 0, 0, -1, 2, 1, 0
  )

  args_lunar_anomaly <- c(
    1, -1, 0, 2, 0, 0, -2, -1, 1, 0, -1, 0, 1, 0, 1, 1, -1, 3, -2,
    -1, 0, -1, 0, 1, 2, 0, -3, -2, -1, -2, 1, 0, 2, 0, -1, 1, 0,
    -1, 2, -1, 1, -2, -1, -1, -2, 0, 1, 4, 0, -2, 0, 2, 1, -2, -3,
    2, 1, -1, 3
  )

  args_moon_node <- c(
    0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, -2, 2, -2, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, -2, 2, 0, 2, 0, 0, 0, 0,
    0, 0, -2, 0, 0, 0, 0, -2, -2, 0, 0, 0, 0, 0, 0, 0
  )

  sine_coefficients <- c(
    6288774, 1274027, 658314, 213618, -185116, -114332,
    58793, 57066, 53322, 45758, -40923, -34720, -30383,
    15327, -12528, 10980, 10675, 10034, 8548, -7888,
    -6766, -5163, 4987, 4036, 3994, 3861, 3665, -2689,
    -2602, 2390, -2348, 2236, -2120, -2069, 2048, -1773,
    -1595, 1215, -1110, -892, -810, 759, -713, -700, 691,
    596, 549, 537, 520, -487, -399, -381, 351, -340, 330,
    327, -323, 299, 294
  )
  parts <- matrix(0, nrow = length(sine_coefficients), ncol = length(u))
  for (i in seq_along(sine_coefficients)) {
    parts[i, ] <- sine_coefficients[i] * (cap_E^abs(args_solar_anomaly[i])) *
      sin_degrees((args_lunar_elongation[i] * cap_D) +
        (args_solar_anomaly[i] * cap_M) +
        (args_lunar_anomaly[i] * cap_M_prime) +
        (args_moon_node[i] * cap_F))
  }
  correction <- (deg(1 / 1000000)) * colSums(parts)

  A1 <- deg(119.75) + (u * deg(131.849))
  venus <- (deg(3958 / 1000000) * sin_degrees(A1))

  A2 <- deg(53.09) + u * deg(479264.29)
  jupiter <- (deg(318 / 1000000) * sin_degrees(A2))

  flat_earth <- (deg(1962 / 1000000) * sin_degrees(cap_L_prime - cap_F))

  (cap_L_prime + correction + venus + jupiter + flat_earth + nutation(tee)) %% 360
}

nth_new_moon <- function(n) {
  # Return the moment of n-th new moon after (or before) the new moon
  # of January 11, 1.  Adapted from "Astronomical Algorithms"
  # by Jean Meeus, Willmann_Bell, Inc., 2nd ed., 1998
  n0 <- 24724
  k <- n - n0
  u <- k / 1236.85
  approx <- (J2000 +
    poly(u, c(
      5.09766,
      MEAN_SYNODIC_MONTH * 1236.85,
      0.0001437,
      -0.000000150,
      0.00000000073
    )))
  cap_E <- poly(u, c(1, -0.002516, -0.0000074))
  solar_anomaly <- poly(u, deg(c(
    2.5534, (1236.85 * 29.10535669), -0.0000014, -0.00000011
  )))
  lunar_anomaly <- poly(u, deg(c(
    201.5643,
    (385.81693528 * 1236.85),
    0.0107582, 0.00001238,
    -0.000000058
  )))
  moon_argument <- poly(u, deg(c(
    160.7108, (390.67050284 * 1236.85), -0.0016118, -0.00000227, 0.000000011
  )))
  cap_omega <- poly(u, c(124.7746, (-1.56375588 * 1236.85), 0.0020672, 0.00000215))

  E_factor <- c(
    0, 1, 0, 0, 1, 1, 2, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0
  )

  solar_coeff <- c(
    0, 1, 0, 0, -1, 1, 2, 0, 0, 1, 0, 1, 1, -1, 2,
    0, 3, 1, 0, 1, -1, -1, 1, 0
  )

  lunar_coeff <- c(
    1, 0, 2, 0, 1, 1, 0, 1, 1, 2, 3, 0, 0, 2, 1, 2,
    0, 1, 2, 1, 1, 1, 3, 4
  )

  moon_coeff <- c(
    0, 0, 0, 2, 0, 0, 0, -2, 2, 0, 0, 2, -2, 0, 0,
    -2, 0, -2, 2, 2, 2, -2, 0, 0
  )

  sine_coeff <- c(
    -0.40720, 0.17241, 0.01608, 0.01039, 0.00739, -0.00514,
    0.00208, -0.00111, -0.00057, 0.00056, -0.00042, 0.00042,
    0.00038, -0.00024, -0.00007, 0.00004, 0.00004, 0.00003,
    0.00003, -0.00003, 0.00003, -0.00002, -0.00002, 0.00002
  )

  parts <- matrix(0, nrow = length(sine_coeff), ncol = length(u))
  for (i in seq_along(sine_coeff)) {
    parts[i, ] <- sine_coeff[i] * (cap_E^abs(E_factor[i])) *
      sin_degrees(solar_coeff[i] * solar_anomaly +
        lunar_coeff[i] * lunar_anomaly +
        moon_coeff[i] * moon_argument)
  }
  correction <- (deg(-0.00017) * sin_degrees(cap_omega)) + colSums(parts)

  add_const <- c(
    251.88, 251.83, 349.42, 84.66, 141.74, 207.14, 154.84, 34.52,
    207.19, 291.34, 161.72, 239.56, 331.55
  )

  add_coeff <- c(
    0.016321, 26.651886, 36.412478, 18.206239, 53.303771, 2.453732,
    7.306860, 27.261239, 0.121824, 1.844379, 24.198154, 25.513099,
    3.592518
  )

  add_factor <- c(
    0.000165, 0.000164, 0.000126, 0.000110, 0.000062, 0.000060,
    0.000056, 0.000047, 0.000042, 0.000040, 0.000037, 0.000035,
    0.000023
  )

  parts <- matrix(0, nrow = length(add_const), ncol = length(u))
  for (i in seq_along(add_const)) {
    parts[i, ] <- add_factor[i] * sin_degrees(add_const[i] + add_coeff[i] * k)
  }
  additional <- colSums(parts)

  extra <- deg(0.000325) *
    sin_degrees(poly(u, deg(c(299.77, 132.8475848, -0.009173))))

  universal_from_dynamical(approx + correction + extra + additional)
}

new_moon_before <- function(tee) {
  # Moment UT of last new moon before tee
  n <- round(tee / MEAN_SYNODIC_MONTH - lunar_phase(tee) / 360)
  nth_new_moon(n)
}

new_moon_after <- function(tee) {
  # Moment UT of first new moon at or after tee
  n <- round(tee / MEAN_SYNODIC_MONTH - lunar_phase(tee) / 360)
  nth_new_moon(n + 1)
}

#' Lunar phase at date
#'
#' Lunar phase at date, as an angle in degrees. An angle of 0 means a new moon,
#' 90 degrees means the first quarter, 180 means a full moon, and 270 degrees
#' means the last quarter.
#'
#' @param date Date vector
#' @param location Location object
#' @param ... Additional arguments
#' @examples
#' april2025 <- seq(as.Date("2025-04-01"), as.Date("2025-04-30"), by = "1 day")
#' melbourne <- location(-37.8136, 144.9631, 31, 10)
#' lunar_phase(april2025, melbourne)
#'
#' @export
#'
lunar_phase <- function(date, location, ...) {
  UseMethod("lunar_phase")
}

#' @export
lunar_phase.default <- function(date, location, ...) {
  lunar_phase(as_rd(date), location)
}

#' @export
lunar_phase.rd_fixed <- function(date, location, ...) {
  # Lunar phase, as an angle in degrees, at moment tee
  # An angle of 0 means a new moon, 90 degrees means the first quarter,
  # 180 means a full moon, and 270 degrees means the last quarter
  tee <- vec_data(date)
  phi <- (lunar_longitude(tee) - solar_longitude(tee)) %% 360
  t0 <- nth_new_moon(0)
  n <- round((tee - t0) / MEAN_SYNODIC_MONTH)
  phi_prime <- deg(360) *
    ((tee - nth_new_moon(n)) / MEAN_SYNODIC_MONTH) %% 1
  idx <- which(abs(phi - phi_prime) > deg(180))
  phi[idx] <- phi_prime[idx]
  return(phi)
}

lunar_latitude <- function(tee) {
  # Return the latitude of moon (in degrees) at moment, tee.
  # Adapted from "Astronomical Algorithms" by Jean Meeus,
  # Willmann_Bell, Inc., 1998
  u <- julian_centuries(tee)
  cap_L_prime <- mean_lunar_longitude(u)
  cap_D <- lunar_elongation(u)
  cap_M <- solar_anomaly(u)
  cap_M_prime <- lunar_anomaly(u)
  cap_F <- moon_node(u)
  cap_E <- poly(u, c(1, -0.002516, -0.0000074))

  args_lunar_elongation <- c(
    0, 0, 0, 2, 2, 2, 2, 0, 2, 0, 2, 2, 2, 2, 2, 2, 2, 0, 4, 0, 0, 0,
    1, 0, 0, 0, 1, 0, 4, 4, 0, 4, 2, 2, 2, 2, 0, 2, 2, 2, 2, 4, 2, 2,
    0, 2, 1, 1, 0, 2, 1, 2, 0, 4, 4, 1, 4, 1, 4, 2
  )

  args_solar_anomaly <- c(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 1, -1, -1, -1, 1, 0, 1,
    0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1, 1,
    0, -1, -2, 0, 1, 1, 1, 1, 1, 0, -1, 0, 0, 0, -1, -2
  )

  args_lunar_anomaly <- c(
    0, 1, 1, 0, -1, -1, 0, 2, 1, 2, 0, -2, 1, 0, -1, 0, -1, -1, -1,
    0, 0, -1, 0, 1, 1, 0, 0, 3, 0, -1, 1, -2, 0, 2, 1, -2, 3, 2, -3,
    -1, 0, 0, 1, 0, 1, 1, 0, 0, -2, -1, 1, -2, 2, -2, -1, 1, 1, -2,
    0, 0
  )

  args_moon_node <- c(
    1, 1, -1, -1, 1, -1, 1, 1, -1, -1, -1, -1, 1, -1, 1, 1, -1, -1,
    -1, 1, 3, 1, 1, 1, -1, -1, -1, 1, -1, 1, -3, 1, -3, -1, -1, 1,
    -1, 1, -1, 1, 1, 1, 1, -1, 3, -1, -1, 1, -1, -1, 1, -1, 1, -1,
    -1, -1, -1, -1, -1, 1
  )

  sine_coefficients <- c(
    5128122, 280602, 277693, 173237, 55413, 46271, 32573,
    17198, 9266, 8822, 8216, 4324, 4200, -3359, 2463, 2211,
    2065, -1870, 1828, -1794, -1749, -1565, -1491, -1475,
    -1410, -1344, -1335, 1107, 1021, 833, 777, 671, 607,
    596, 491, -451, 439, 422, 421, -366, -351, 331, 315,
    302, -283, -229, 223, 223, -220, -220, -185, 181,
    -177, 176, 166, -164, 132, -119, 115, 107
  )

  parts <- matrix(0, nrow = length(sine_coefficients), ncol = length(u))
  for (i in seq_along(sine_coefficients)) {
    parts[i, ] <- sine_coefficients[i] *
      cap_E^abs(args_solar_anomaly[i]) *
      sin_degrees((args_lunar_elongation[i] * cap_D) +
        (args_solar_anomaly[i] * cap_M) +
        (args_lunar_anomaly[i] * cap_M_prime) +
        (args_moon_node[i] * cap_F))
  }
  beta <- deg(1 / 1000000) * colSums(parts)

  venus <- (deg(175 / 1000000) *
    (sin_degrees(deg(119.75) + c * deg(131.849) + cap_F) +
      sin_degrees(deg(119.75) + c * deg(131.849) - cap_F)))

  flat_earth <- (deg(-2235 / 1000000) * sin_degrees(cap_L_prime) +
    deg(127 / 1000000) * sin_degrees(cap_L_prime - cap_M_prime) +
    deg(-115 / 1000000) * sin_degrees(cap_L_prime + cap_M_prime))

  extra <- (deg(382 / 1000000) *
    sin_degrees(deg(313.45) + c * deg(481266.484)))

  return(beta + venus + flat_earth + extra)
}

lunar_altitude <- function(tee, locale) {
  # Altitude of moon at tee at locale, ignoring parallax and refraction
  # Adapted from "Astronomical Algorithms" by Jean Meeus, Willmann-Bell, Inc., 1998
  phi <- latitude(location)
  psi <- longitude(location)
  lamb <- lunar_longitude(tee)
  beta <- lunar_latitude(tee)
  alpha <- right_ascension(tee, beta, lamb)
  delta <- declination(tee, beta, lamb)
  theta0 <- sidereal_from_moment(tee)
  cap_H <- (theta0 + psi - alpha) %% 360
  altitude <- arcsin_degrees(
    (sin_degrees(phi) * sin_degrees(delta)) +
      (cosine_degrees(phi) * cosine_degrees(delta) * cosine_degrees(cap_H))
  )
  (altitude + deg(180)) %% 360 - deg(180)
}


visible_crescent <- function(date, locale) {
  # S. K. Shaukat's criterion for likely visibility of crescent moon on date at locale
  # Best viewing time
  tee <- universal_from_standard(dusk(date, locale, deg(4.5)), locale)

  phase <- lunar_phase(tee)
  altitude <- lunar_altitude(tee, locale)

  # Angular separation of sun and moon
  arc_of_light <- arccos_degrees(
    cosine_degrees(lunar_latitude(tee)) * cosine_degrees(phase)
  )

  NEW < phase &&
    phase < FIRST_QUARTER &&
    deg(10.6) <= arc_of_light &&
    arc_of_light <= deg(90) &&
    altitude > deg(4.1)
}

phasis_on_or_before <- function(date, locale) {
  # Closest fixed date on or before date when crescent moon first became visible at locale
  # Mean date of prior new moon
  mean <- date - floor(lunar_phase(date + 1) / 360 * MEAN_SYNODIC_MONTH)

  # Check if not visible yet on date
  tau <- if (date - mean <= 3 && !visible_crescent(date, locale)) {
    mean - 30 # Must go back a month
  } else {
    mean - 2
  }

  # Search for first visible date
  next_value(tau, function(d) {
    visible_crescent(d, locale)
  })
}


declination <- function(tee, beta, lam) {
  # Return declination at moment UT tee of object at
  # longitude 'lam' and latitude 'beta'
  varepsilon <- obliquity(tee)
  return(arcsin_degrees(
    (sin_degrees(beta) * cosine_degrees(varepsilon)) +
      (cosine_degrees(beta) * sin_degrees(varepsilon) * sin_degrees(lam))
  ))
}

lunar_phase_after <- function(tee, phi) {
  # Moment UT of the next time at or after tee when the lunar-phase is phi degrees
  varepsilon <- 1e-5 # Accuracy

  # Estimate
  tau <- tee +
    MEAN_SYNODIC_MONTH * (1 / 360) * ((phi - lunar_phase(tee)) %% 360)

  l <- pmax(tee, tau - 2) # At or after tee
  u <- tau + 2

  lst <- vctrs::vec_cast_common(l = l, u = u, phi = phi)
  output <- rep(0, length(lst$l))
  for (i in seq_along(output)) {
    output[i] <- binary_search(
      lst$l[i], lst$u[i],
      function(l, u) {
        (u - l) < varepsilon
      },
      function(x) {
        ((lunar_phase(x) - lst$phi[i]) %% 360) < 180
      }
    )
  }
  output
}


solar_longitude_after <- function(tee, phi) {
  # Moment UT of the first time at or after tee when the solar longitude will be phi degrees
  varepsilon <- 1e-5 # Accuracy of solar-longitude

  # Mean days for 1 degree change
  rate <- MEAN_TROPICAL_YEAR / 360

  # Estimate (within 5 days)
  tau <- tee + rate * ((phi - solar_longitude(tee)) %% 360)

  l <- pmax(tee, tau - 5) # At or after tee
  u <- tau + 5

  lst <- vctrs::vec_cast_common(l = l, u = u, phi = phi)
  output <- rep(0, length(lst$l))
  for (i in seq_along(output)) {
    output[i] <- binary_search(
      lst$l[i], lst$u[i],
      function(l, u) {
        (u - l) < varepsilon
      },
      function(x) {
        ((solar_longitude(x) - lst$phi[i]) %% 360) < 180
      }
    )
  }
  output
}

invert_angular <- function(f, y, a, b, prec = 1e-5) {
  # Find inverse of angular function 'f' at 'y' within interval [a,b].
  # Default precision is 0.00001
  # Bisection search
  lst <- vctrs::vec_cast_common(y = y, a = a, b = b)
  output <- rep(0, length(lst$y))
  for (i in seq_along(output)) {
    output[i] <- binary_search(
      lst$a[i], lst$b[i],
      function(l, h) ((h - l) <= prec),
      function(x) ((f(x) - lst$y[i]) %% 360) < 180
    )
  }
  output
}

right_ascension <- function(tee, beta, lam) {
  # Return right ascension at moment UT 'tee' of object at
  # latitude 'lam' and longitude 'beta'
  varepsilon <- obliquity(tee)
  arctan_degrees(
    (sin_degrees(lam) * cosine_degrees(varepsilon)) -
      (tangent_degrees(beta) * sin_degrees(varepsilon)),
    cosine_degrees(lam)
  )
}
