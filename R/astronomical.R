# ==============================================================================
# Section: Time and Astronomy
# ==============================================================================

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
  alpha[!quad %in% c(1,4)] <- alpha[!quad %in% c(1,4)] + 180
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
  u <- julian_centuries(tee)
  
  angle(23, 26, 21.448) + 
    poly(u, c(0, angle(0, 0, -46.8150), angle(0, 0, -0.00059), angle(0, 0, 0.001813)))
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
  sine_offset <- tangent_degrees(phi) * tangent_degrees(delta) +
    sin_degrees(alpha) / (cosine_degrees(delta) * cosine_degrees(phi))
  result <- local_from_apparent(
    floor(approx) + 0.5 + (1 - 2*morning) *
      ((0.5 + arcsin_degrees(sine_offset) / 360) %% 1 - 0.25)
  )
  result[abs(sine_offset) > 1] <- NA_real_
  return(result)
}

dawn <- function(date, locale, alpha) {
  # Standard time in morning of date at locale when depression angle of sun is alpha
  # First, get an approximate time
  approx <- moment_from_depression(date + 0.25, locale, alpha)
  result <- approx
  result[is.na(approx)] <- moment_from_depression(date, locale, alpha)
  result[!is.na(approx)] <- moment_from_depression(approx, locale, alpha)
  result[!is.na(result)] <- standard_from_local(result[!is.na(result)], locale)
  
  return(result)
}

dusk <- function(date, locale, alpha) {
  # Standard time in evening on date at locale when depression angle of sun is alpha
  # First, get an approximate time
  approx <- moment_from_depression(date + 0.75, locale, alpha)
  approx[is.na(approx)] <- date[is.na(approx)] + 0.99
  
  # Then refine the calculation
  result <- moment_from_depression(approx, locale, alpha)
  
  standard_from_local(result, locale)
}

sunrise <- function(date, locale) {
  # Standard time of sunrise on date at locale
  h <- max(0, elevation(locale))
  cap_R <- mt(6.372e6)  # Radius of Earth
  
  # Depression of visible horizon
  dip <- arccos_degrees(cap_R / (cap_R + h))
  
  alpha <- angle(0, 50, 0) + dip
  
  dawn(date, locale, alpha)
}

sunset <- function(date, locale) {
  # Standard time of sunset on fixed date at locale
  h <- max(0, elevation(locale))
  cap_R <- mt(6.372e6)  # Radius of Earth
  
  # Depression of visible horizon
  dip <- arccos_degrees(cap_R / (cap_R + h))
  
  alpha <- angle(0, 50, 0) + dip
  
  dusk(date, locale, alpha)
}

jewish_dusk <- function(date, locale) {
  # Standard time of Jewish dusk on fixed date at locale (as per Vilna Gaon)
  dusk(date, locale, angle(4, 40, 0))
}

jewish_sabbath_ends <- function(date, locale) {
  # Standard time of end of Jewish sabbath on fixed date at locale (as per Berthold Cohn)
  dusk(date, locale, angle(7, 5, 0))
}

temporal_hour <- function(date, locale) {
  # Length of daytime temporal hour on fixed date at locale
    (sunset(date, locale) - sunrise(date, locale)) / 12
}

standard_from_sundial <- function(date, hour, locale) {
  # Standard time on fixed date of temporal hour at locale
  tee <- temporal_hour(date, locale)
  result <- sunrise(date, locale)
  daylight <- 6 <= hour & hour <= 18
  result[daylight] <- result[daylight] + (hour[daylight] - 6) * tee[daylight]
  result[!daylight] <- result[!daylight] + (hour[!daylight] - 6) * (1/12 - tee[!daylight])
  result
}

jewish_morning_end <- function(date, locale) {
  # Standard time on fixed date at locale of end of morning according to Jewish ritual
  standard_from_sundial(date, 10, locale)
}

asr <- function(date, locale) {
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
    sin_degrees(phi) * sin_degrees(delta) +
      cosine_degrees(phi) * cosine_degrees(delta)
  )
  
  # Sun's altitude when shadow increases by double its length
  h <- arctan_degrees(
    tangent_degrees(altitude) / (1 + 2 * tangent_degrees(altitude)),
    1
  )
  
  dusk(date, locale, -h)
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
  
  deg(poly(u, c(280.46061837, 36525 * 360.98564736629, 0.000387933, -1/38710000))) %% 360
}

ephemeris_correction <- function(tee) {
  # Dynamical Time minus Universal Time (in days) for fixed time tee
  # Adapted from "Astronomical Algorithms" by Jean Meeus, Willmann-Bell, Inc., 1991
  year <- gregorian_year_from_fixed(floor(tee))
  
  u <- (as_rd(gregorian(year, JULY, 1)) -  as_rd(gregorian(1900, JANUARY, 1))) / 36525

  result1 <- (year - 1933) / (24 * 60 * 60)
  result2 <- poly(u, c(-0.00002, 0.000297, 0.025184, -0.181133, 0.553040, -0.861938, 0.677066, -0.212591))
  result3 <- poly(u, c(-0.000009, 0.003844, 0.083563, 0.865736, 4.867575, 15.845535, 31.332267, 38.291999, 28.316289, 11.636204, 2.043794))
  result4 <- poly(year - 1600, c(196.58333, -4.0675, 0.0219167)) / (24 * 60 * 60)
  x <- hr(12) + as_rd(gregorian(year, JANUARY, 1)) -  as_rd(gregorian(1810, JANUARY, 1))
  result <- (x * x / 41048480 - 15) / (24 * 60 * 60)

  result[1988 <= year & year <= 2019] <- result1[1988 <= year && year <= 2019]
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
  
  equation <- 1 / (2 * pi) * (
    y * sin_degrees(2 * longitude) +
      -2 * eccentricity * sin_degrees(anomaly) +
      4 * eccentricity * y * sin_degrees(anomaly) * cosine_degrees(2 * longitude) +
      -0.5 * y * y * sin_degrees(4 * longitude) +
      -1.25 * eccentricity * eccentricity * sin_degrees(2 * anomaly)
  )
  
  sign(equation) * min(abs(equation), hr(12))
}

solar_longitude <- function(tee) {
  # Longitude of sun at moment tee
  # Adapted from "Planetary Programs and Tables from -4000 to +2800" 
  # by Pierre Bretagnon and Jean-Louis Simon, Willmann-Bell, Inc., 1986
  c <- julian_centuries(tee)
  
  coefficients <- c(403406, 195207, 119433, 112392, 3891, 2819, 1721,
                  660, 350, 334, 314, 268, 242, 234, 158, 132, 129, 114,
                  99, 93, 86, 78, 72, 68, 64, 46, 38, 37, 32, 29, 28, 27, 27,
                  25, 24, 21, 21, 20, 18, 17, 14, 13, 13, 13, 12, 10, 10, 10, 10)
  
  multipliers <- c(0.9287892, 35999.1376958, 35999.4089666,
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
                 90073.778)
  
  addends <- c(270.54861, 340.19128, 63.91854, 331.26220,
             317.843, 86.631, 240.052, 310.26, 247.23,
             260.87, 297.82, 343.14, 166.79, 81.53,
             3.50, 132.75, 182.95, 162.03, 29.8,
             266.4, 249.2, 157.6, 257.8, 185.1, 69.9,
             8.0, 197.1, 250.4, 65.3, 162.7, 341.5,
             291.6, 98.5, 146.7, 110.0, 5.2, 342.6,
             230.9, 256.1, 45.3, 242.9, 115.2, 151.8,
             285.3, 53.3, 126.6, 205.7, 85.9,
             146.1)
  
  # Calculate the sum part
  sum_part <- 0
  for (i in 1:length(coefficients)) {
    sum_part <- sum_part + coefficients[i] * 
      sin_degrees(addends[i] + multipliers[i] * c)
  }
  
  longitude <- deg(282.7771834) + 
    36000.76953744 * c + 
    0.000005729577951308232 * sum_part
  
  (longitude + aberration(tee) + nutation(tee)) %% 360
}

nutation <- function(tee) {
  # Longitudinal nutation at moment tee
  u <- julian_centuries(tee)
  
  cap_A <- poly(u, deg(c(124.90, -1934.134, 0.002063)))
  cap_B <- poly(u, deg(c(201.11, 72001.5377, 0.00057)))
  
  deg(-0.004778) * sin_degrees(cap_A) + 
    deg(-0.0003667) * sin_degrees(cap_B)
}

aberration <- function(tee) {
  # Aberration at moment tee
  c <- julian_centuries(tee)
  
  deg(0.0000974) * cosine_degrees(deg(177.63) + deg(35999.01848) * c) - deg(0.005575)
}

solar_longitude_after <- function(tee, phi) {
  # Moment UT of the first time at or after tee when the solar longitude will be phi degrees
  varepsilon <- 1e-5  # Accuracy of solar-longitude
  
  # Mean days for 1 degree change
  rate <- MEAN_TROPICAL_YEAR / 360
  
  # Estimate (within 5 days)
  tau <- tee + rate * ((phi - solar_longitude(tee)) %% 360)
  
  l <- max(tee, tau - 5)  # At or after tee
  u <- tau + 5
  
  # Bisection search
  binary_search(
    l, u,
    function(x) {
      ((solar_longitude(x) - phi) %% 360) < 180
    },
    function(l, u) {
      (u - l) < varepsilon
    }
  )
}


lunar_longitude <- function(tee) {
  # Longitude of moon (in degrees) at moment tee
  # Adapted from "Astronomical Algorithms" by Jean Meeus, Willmann-Bell, Inc., 1991
  u <- julian_centuries(tee)
  
  mean_moon <- degrees(
    poly(u, deg(c(218.3164591, 481267.88134236, -0.0013268, 1/538841, -1/65194000)))
  )
  
  elongation <- degrees(
    poly(u, deg(c(297.8502042, 445267.1115168, -0.00163, 1/545868, -1/113065000)))
  )
  
  solar_anomaly <- degrees(
    poly(u, deg(c(357.5291092, 35999.0502909, -0.0001536, 1/24490000)))
  )
  
  lunar_anomaly <- degrees(
    poly(u, deg(c(134.9634114, 477198.8676313, 0.008997, 1/69699, -1/14712000)))
  )
  
  moon_node <- degrees(
    poly(u, deg(c(93.2720993, 483202.0175273, -0.0034029, -1/3526000, 1/863310000)))
  )
  
  cap_E <- poly(u, c(1, -0.002516, -0.0000074))
  
  args_lunar_elongation <- c(0, 2, 2, 0, 0, 0, 2, 2, 2, 2, 0, 1, 0, 2, 0, 0, 4, 0, 4, 2, 2, 1,
                           1, 2, 2, 4, 2, 0, 2, 2, 1, 2, 0, 0, 2, 2, 2, 4, 0, 3, 2, 4, 0, 2,
                           2, 2, 4, 0, 4, 1, 2, 0, 1, 3, 4, 2, 0, 1, 2)
  
  args_solar_anomaly <- c(0, 0, 0, 0, 1, 0, 0, -1, 0, -1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1,
                        0, 1, -1, 0, 0, 0, 1, 0, -1, 0, -2, 1, 2, -2, 0, 0, -1, 0, 0, 1,
                        -1, 2, 2, 1, -1, 0, 0, -1, 0, 1, 0, 1, 0, 0, -1, 2, 1, 0)
  
  args_lunar_anomaly <- c(1, -1, 0, 2, 0, 0, -2, -1, 1, 0, -1, 0, 1, 0, 1, 1, -1, 3, -2,
                        -1, 0, -1, 0, 1, 2, 0, -3, -2, -1, -2, 1, 0, 2, 0, -1, 1, 0,
                        -1, 2, -1, 1, -2, -1, -1, -2, 0, 1, 4, 0, -2, 0, 2, 1, -2, -3,
                        2, 1, -1, 3)
  
  args_moon_node <- c(0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, -2, 2, -2, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, -2, 2, 0, 2, 0, 0, 0, 0,
                    0, 0, -2, 0, 0, 0, 0, -2, -2, 0, 0, 0, 0, 0, 0, 0)
  
  sine_coefficients <- c(6288774, 1274027, 658314, 213618, -185116, -114332,
                       58793, 57066, 53322, 45758, -40923, -34720, -30383,
                       15327, -12528, 10980, 10675, 10034, 8548, -7888,
                       -6766, -5163, 4987, 4036, 3994, 3861, 3665, -2689,
                       -2602, 2390, -2348, 2236, -2120, -2069, 2048, -1773,
                       -1595, 1215, -1110, -892, -810, 759, -713, -700, 691,
                       596, 549, 537, 520, -487, -399, -381, 351, -340, 330,
                       327, -323, 299, 294)
  
  # Calculate the correction
  correction <- 0
  for (i in 1:length(sine_coefficients)) {
    correction <- correction + 
      sine_coefficients[i] * cap_E^abs(args_solar_anomaly[i]) * 
      sin_degrees(
        args_lunar_elongation[i] * elongation +
          args_solar_anomaly[i] * solar_anomaly +
          args_lunar_anomaly[i] * lunar_anomaly +
          args_moon_node[i] * moon_node
      )
  }
  correction <- correction * deg(1) / 1000000
  
  venus <- deg(3958) / 1000000 * 
    sin_degrees(119.75 + c * 131.849)
  
  jupiter <- deg(318) / 1000000 * 
    sin_degrees(53.09 + c * 479264.29)
  
  flat_earth <- deg(1962) / 1000000 * 
    sin_degrees(mean_moon - moon_node)
  
  (mean_moon + correction + venus + jupiter + flat_earth + nutation(tee)) %% 360
}

nth_new_moon <- function(n) {
  # Moment of n-th new moon after (or before) the new moon of January 11, 1
  # Adapted from "Astronomical Algorithms" by Jean Meeus, Willmann-Bell, Inc., 1991
  k <- n - 24724  # Months since j2000
  u <- k / 1236.85  # Julian centuries
  
  approx <- poly(u, c(730125.59765,
                       MEAN_SYNODIC_MONTH * 1236.85,
                       0.0001337,
                       -0.000000150,
                       0.00000000073))
  
  cap_E <- poly(u, c(1, -0.002516, -0.0000074))
  
  solar_anomaly <- poly(u, deg(c(2.5534,
                                1236.85 * 29.10535669,
                                -0.0000218, -0.00000011)))
  
  lunar_anomaly <- poly(u, deg(c(201.5643, 385.81693528 * 1236.85,
                               0.0107438, 0.00001239,
                               -0.000000058)))
  
  # Moon's argument of latitude
  moon_argument <- poly(u, deg(c(160.7108, 390.67050274 * 1236.85,
                              -0.0016341, -0.00000227,
                              0.000000011)))
  
  # Longitude of ascending node
  cap_omega <- poly(u, c(124.7746, -1.56375580 * 1236.85,
                          0.0020691, 0.00000215))
  
  E_factor <- c(0, 1, 0, 0, 1, 1, 2, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0)
  
  solar_coeff <- c(0, 1, 0, 0, -1, 1, 2, 0, 0, 1, 0, 1, 1, -1, 2,
                 0, 3, 1, 0, 1, -1, -1, 1, 0)
  
  lunar_coeff <- c(1, 0, 2, 0, 1, 1, 0, 1, 1, 2, 3, 0, 0, 2, 1, 2,
                 0, 1, 2, 1, 1, 1, 3, 4)
  
  moon_coeff <- c(0, 0, 0, 2, 0, 0, 0, -2, 2, 0, 0, 2, -2, 0, 0,
                -2, 0, -2, 2, 2, 2, -2, 0, 0)
  
  sine_coeff <- c(-0.40720, 0.17241, 0.01608, 0.01039,
                0.00739, -0.00514, 0.00208,
                -0.00111, -0.00057, 0.00056,
                -0.00042, 0.00042, 0.00038,
                -0.00024, -0.00007, 0.00004,
                0.00004, 0.00003, 0.00003,
                -0.00003, 0.00003, -0.00002,
                -0.00002, 0.00002)
  
  # Calculate the correction
  correction <- deg(-0.00017) * sin_degrees(cap_omega)
  for (i in 1:length(sine_coeff)) {
    correction <- correction + 
      sine_coeff[i] * cap_E^E_factor[i] * 
      sin_degrees(
        solar_coeff[i] * solar_anomaly +
          lunar_coeff[i] * lunar_anomaly +
          moon_coeff[i] * moon_argument
      )
  }
  
  add_const <- c(251.88, 251.83, 349.42, 84.66,
               141.74, 207.14, 154.84, 34.52, 207.19,
               291.34, 161.72, 239.56, 331.55)
  
  add_coeff <- c(0.016321, 26.641886,
               36.412478, 18.206239, 53.303771,
               2.453732, 7.306860, 27.261239, 0.121824,
               1.844379, 24.198154, 25.513099,
               3.592518)
  
  add_factor <- c(0.000165, 0.000164, 0.000126,
                0.000110, 0.000062, 0.000060, 0.000056,
                0.000047, 0.000042, 0.000040, 0.000037,
                0.000035, 0.000023)
  
  extra <- deg(0.000325) * 
    sin_degrees(poly(u, deg(c(299.77, 132.8475848, -0.009173))))
  
  # Calculate additional
  additional <- 0
  for (i in 1:length(add_const)) {
    additional <- additional + 
      add_factor[i] * sin_degrees(add_const[i] + add_coeff[i] * k)
  }
  
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

lunar_phase <- function(tee) {
  # Lunar phase, as an angle in degrees, at moment tee
  # An angle of 0 means a new moon, 90 degrees means the first quarter, 
  # 180 means a full moon, and 270 degrees means the last quarter
  (lunar_longitude(tee) - solar_longitude(tee)) %% 360
}

lunar_phase_before <- function(tee, phi) {
  # Moment UT of the last time at or before tee when the lunar-phase was phi degrees
  varepsilon <- 1e-5  # Accuracy
  
  # Estimate
  tau <- tee - MEAN_SYNODIC_MONTH * (1/360) * ((lunar_phase(tee) - phi) %% 360)
  
  l <- tau - 2
  u <- min(tee, tau + 2)  # At or before tee
  
  # Bisection search
  binary_search(
    l, u,
    function(x) {
      ((lunar_phase(x) - phi) %% 360) < 180
    },
    function(l, u) {
      (u - l) < varepsilon
    }
  )
}


lunar_phase_after <- function(tee, phi) {
  # Moment UT of the next time at or after tee when the lunar-phase is phi degrees
  varepsilon <- 1e-5  # Accuracy
  
  # Estimate
  tau <- tee + MEAN_SYNODIC_MONTH * (1/360) * ((phi - lunar_phase(tee)) %% 360)
  
  l <- max(tee, tau - 2)  # At or after tee
  u <- tau + 2
  
  # Bisection search
  binary_search(
    l, u,
    function(x) {
      ((lunar_phase(x) - phi) %% 360) < 180
    },
    function(l, u) {
      (u - l) < varepsilon
    }
  )
}

lunar_latitude <- function(tee) {
  # Latitude of moon (in degrees) at moment tee
  # Adapted from "Astronomical Algorithms" by Jean Meeus, Willmann-Bell, Inc., 1998
  u <- julian_centuries(tee)
  
  longitude <- degrees(
    poly(u, deg(c(218.3164591, 481267.88134236, -0.0013268, 1/538841, -1/65194000)))
  )
  
  elongation <- degrees(
    poly(u, deg(c(297.8502042, 445267.1115168, -0.00163, 1/545868, -1/113065000)))
  )
  
  solar_anomaly <- degrees(
    poly(u, deg(c(357.5291092, 35999.0502909, -0.0001536, 1/24490000)))
  )
  
  lunar_anomaly <- degrees(
    poly(u, deg(c(134.9634114, 477198.8676313, 0.008997, 1/69699, -1/14712000)))
  )
  
  moon_node <- degrees(
    poly(u, deg(c(93.2720993, 483202.0175273, -0.0034029, -1/3526000, 1/863310000)))
  )
  
  cap_E <- poly(u, c(1, -0.002516, -0.0000074))
  
  args_lunar_elongation <- c(0, 0, 0, 2, 2, 2, 2, 0, 2, 2, 2, 2, 2, 2, 2, 0, 4, 0, 0, 0,
                           1, 0, 0, 0, 1, 0, 4, 4, 0, 4, 2, 2, 2, 2, 0, 2, 2, 2, 2, 4, 2, 2,
                           0, 2, 1, 1, 0, 2, 1, 2, 0, 4, 4, 1, 4, 2)
  
  args_solar_anomaly <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 1, -1, -1, -1, 1, 0, 1,
                        0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1, 1,
                        0, -1, -2, 0, 1, 1, 1, 1, 1, 0, -1, 1, 0, -1, 0, 0, 0, -1, -2)
  
  args_lunar_anomaly <- c(0, 1, 1, 0, -1, -1, 0, 2, 1, 2, 0, -2, 1, 0, -1, 0, -1, -1, -1,
                        0, 0, -1, 0, 1, 1, 0, 0, 3, 0, -1, 1, -2, 0, 2, 1, -2, 3, 2, -3,
                        -1, 0, 0, 1, 0, 1, 1, 0, 0, -2, -1, 1, -2, 2, -2, -1, 1, 1, -2,
                        0, 0)
  
  args_moon_node <- c(1, 1, -1, -1, 1, -1, 1, 1, -1, -1, -1, -1, 1, -1, 1, 1, -1, -1,
                    -1, 1, 3, 1, 1, 1, -1, -1, -1, 1, -1, 1, -3, 1, -3, -1, -1, 1,
                    -1, 1, -1, 1, 1, 1, 1, -1, 3, -1, -1, 1, -1, -1, 1, -1,
                    -1, -1, -1, -1, -1, 1)
  
  sine_coefficients <- c(5128122, 280602, 277693, 173237, 55413, 46271, 32573,
                       17198, 9266, 8822, 8216, 4324, 4200, -3359, 2463, 2211,
                       2065, -1870, 1828, -1794, -1749, -1565, -1491, -1475,
                       -1410, -1344, -1335, 1107, 1021, 833, 777, 671, 607,
                       596, 491, -451, 439, 422, 421, -366, -351, 331, 315,
                       302, -283, -229, 223, 223, -220, -220, -185, 181,
                       -177, 176, 166, -164, 132, -119, 115, 107)
  
  # Calculate the latitude
  latitude <- 0
  for (i in 1:length(sine_coefficients)) {
    latitude <- latitude + 
      sine_coefficients[i] * cap_E^abs(args_solar_anomaly[i]) * 
      sin_degrees(
        args_lunar_elongation[i] * elongation +
          args_solar_anomaly[i] * solar_anomaly +
          args_lunar_anomaly[i] * lunar_anomaly +
          args_moon_node[i] * moon_node
      )
  }
  latitude <- latitude * deg(1) / 1000000
  
  venus <- deg(175) / 1000000 * (
    sin_degrees(119.75 + c * 131.849 + moon_node) +
      sin_degrees(119.75 + c * 131.849 - moon_node)
  )
  
  flat_earth <- deg(-2235) / 1000000 * sin_degrees(longitude) +
    deg(127) / 1000000 * sin_degrees(longitude - lunar_anomaly) +
    deg(-115) / 1000000 * sin_degrees(longitude + lunar_anomaly)
  
  extra <- deg(382) / 1000000 * 
    sin_degrees(313.45 + c * 481266.484)
  
  (latitude + venus + flat_earth + extra) %% 360
}

lunar_altitude <- function(tee, locale) {
  # Altitude of moon at tee at locale, ignoring parallax and refraction
  # Adapted from "Astronomical Algorithms" by Jean Meeus, Willmann-Bell, Inc., 1998
  phi <- latitude(locale)  # Local latitude
  psi <- longitude(locale)  # Local longitude
  varepsilon <- obliquity(tee)  # Obliquity of ecliptic
  lambda <- lunar_longitude(tee)  # Lunar longitude
  beta <- lunar_latitude(tee)  # Lunar latitude
  
  # Lunar right ascension
  alpha <- arctan_degrees(
    (sin_degrees(lambda) * cosine_degrees(varepsilon) - 
       tangent_degrees(beta) * sin_degrees(varepsilon)) / 
      cosine_degrees(lambda),
    1 + quotient(lambda, 90)  # Quadrant
  )
  
  # Lunar declination
  delta <- arcsin_degrees(
    sin_degrees(beta) * cosine_degrees(varepsilon) +
      cosine_degrees(beta) * sin_degrees(varepsilon) * sin_degrees(lambda)
  )
  
  theta0 <- sidereal_from_moment(tee)  # Sidereal time
  
  # Local hour angle
  cap_H <- (theta0 - psi - alpha) %% 360
  
  # Calculate the altitude
  altitude <- arcsin_degrees(
    sin_degrees(phi) * sin_degrees(delta) +
      cosine_degrees(phi) * cosine_degrees(delta) * cosine_degrees(cap_H)
  )
  
  ((altitude + 180) %% 360) - 180
}

estimate_prior_solar_longitude <- function(tee, phi) {
  # Approximate moment at or before tee when solar longitude just exceeded phi degrees
  # Mean change of one degree
  rate <- MEAN_TROPICAL_YEAR / 360
  
  # First approximation
  tau <- tee - rate * ((solar_longitude(tee) - phi) %% 360)
  
  # Difference in longitude
  cap_Delta <- ((solar_longitude(tau) - phi - 180) %% 360) - 180
  
  min(tee, tau - rate * cap_Delta)
}

sunset_in_haifa <- function(date) {
  # Universal time of sunset of evening before fixed date in Haifa
  universal_from_standard(sunset(date - 1, HAIFA), HAIFA)
}

future_bahai_new_year_on_or_before <- function(date) {
  # Fixed date of Future Bahai New Year on or before fixed date
  # Approximate time of equinox
  approx <- estimate_prior_solar_longitude(sunset_in_haifa(date), SPRING)
  
  next_value(floor(approx) - 1, function(day) {
    solar_longitude(sunset_in_haifa(day)) <= SPRING + 2
  })
}

fixed_from_future_bahai <- function(b_date) {
  # Fixed date of Bahai date
  major <- bahai_major(b_date)
  cycle <- bahai_cycle(b_date)
  year <- bahai_year(b_date)
  month <- bahai_month(b_date)
  day <- bahai_day(b_date)
  
  # Years from epoch
  years <- 361 * (major - 1) + 19 * (cycle - 1) + year
  
  if (month == 19) {
    # last month of year
    future_bahai_new_year_on_or_before(
      BAHAI_EPOCH + floor(MEAN_TROPICAL_YEAR * (years + 1/2))
    ) - 19 + day - 1
  } else if (month == AYYAM_I_HA) {
    # intercalary month, between 18th & 19th
    future_bahai_new_year_on_or_before(
      BAHAI_EPOCH + floor(MEAN_TROPICAL_YEAR * (years - 1/2))
    ) + 342 + day - 1
  } else {
    future_bahai_new_year_on_or_before(
      BAHAI_EPOCH + floor(MEAN_TROPICAL_YEAR * (years - 1/2))
    ) + (month - 1) * 19 + day - 1
  }
}

future_bahai_from_fixed <- function(date) {
  # Future Bahai date corresponding to fixed date
  new_year <- future_bahai_new_year_on_or_before(date)
  years <- round((new_year - BAHAI_EPOCH) / MEAN_TROPICAL_YEAR)
  
  major <- 1 + quotient(years, 361)
  cycle <- 1 + quotient(years %% 361, 19)
  year <- 1 + (years %% 19)
  
  # Days since start of year
  days <- date - new_year
  
  # Determine month
  if (date >= fixed_from_future_bahai(bahai_date(major, cycle, year, 19, 1))) {
    month <- 19  # last month of year
  } else if (date >= fixed_from_future_bahai(bahai_date(major, cycle, year, AYYAM_I_HA, 1))) {
    month <- AYYAM_I_HA  # intercalary month
  } else {
    month <- 1 + quotient(days, 19)
  }
  
  # Determine day
  day <- date - fixed_from_future_bahai(bahai_date(major, cycle, year, month, 1)) + 1
  
  bahai_date(major, cycle, year, month, day)
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
  
  NEW < phase && phase < FIRST_QUARTER &&
    deg(10.6) <= arc_of_light && arc_of_light <= deg(90) &&
    altitude > deg(4.1)
}

phasis_on_or_before <- function(date, locale) {
  # Closest fixed date on or before date when crescent moon first became visible at locale
  # Mean date of prior new moon
  mean <- date - floor(lunar_phase(date + 1) / 360 * MEAN_SYNODIC_MONTH)
  
  # Check if not visible yet on date
  tau <- if (date - mean <= 3 && !visible_crescent(date, locale)) {
    mean - 30  # Must go back a month
  } else {
    mean - 2
  }
  
  # Search for first visible date
  next_value(tau, function(d) {
    visible_crescent(d, locale)
  })
}


fixed_from_observational_islamic <- function(i_date) {
  # Fixed date equivalent to Observational Islamic date
  month <- standard_month(i_date)
  day <- standard_day(i_date)
  year <- standard_year(i_date)
  
  # Middle of given month
  midmonth <- ISLAMIC_EPOCH + floor(((year - 1) * 12 + month - 1/2) * MEAN_SYNODIC_MONTH)
  
  # First day of month
  phasis_on_or_before(midmonth, ISLAMIC_LOCALE) + day - 1
}

observational_islamic_from_fixed <- function(date) {
  # Observational Islamic date (year month day) corresponding to fixed date
  # Most recent new moon
  crescent <- phasis_on_or_before(date, ISLAMIC_LOCALE)
  
  elapsed_months <- round((crescent - ISLAMIC_EPOCH) / MEAN_SYNODIC_MONTH)
  
  year <- 1 + quotient(elapsed_months, 12)
  month <- 1 + (elapsed_months %% 12)
  day <- 1 + (date - crescent)
  
  islamic_date(year, month, day)
}

astronomical_easter <- function(g_year) {
  # Date of (proposed) astronomical Easter in Gregorian year
  # Beginning of year
  jan1 <- as_rd(gregorian(g_year, JANUARY, 1))
  
  # Spring equinox
  equinox <- solar_longitude_after(jan1, SPRING)
  
  # Date of next full moon
  paschal_moon <- floor(
    apparent_from_local(
      local_from_universal(
        lunar_phase_after(equinox, FULL),
        JERUSALEM
      )
    )
  )
  
  # Return the Sunday following the Paschal moon
  kday_after(paschal_moon, SUNDAY)
}

classical_passover_eve <- function(g_year) {
  # Fixed date of Classical (observational) Passover Eve (Nisan 14) occurring in Gregorian year
  jan1 <- as_rd(gregorian(g_year, JANUARY, 1))
  
  # Date (UT) of spring of g_year
  equinox <- solar_longitude_after(jan1, SPRING)
  
  # First possible new moon
  new_moon <- phasis_on_or_before(floor(equinox) + 10, JERUSALEM)
  
  # Time (UT) of sunset at end of 15th
  set <- universal_from_standard(sunset(new_moon + 14, JERUSALEM), JERUSALEM)
  
  # First day of Nisan
  nisan1 <- if (equinox < set) {
    # Spring starts before end of 15th
    new_moon
  } else {
    # Otherwise next month
    phasis_on_or_before(new_moon + 45, JERUSALEM)
  }
  
  nisan1 + 13  # Passover Eve
}
