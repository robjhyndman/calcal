# ==============================================================================
# Solar and Lunar Functions
# ==============================================================================

universal_from_dynamical <- function(tee) {
  tee - ephemeris_correction(tee)
}

dynamical_from_universal <- function(tee_rom_u) {
  tee_rom_u + ephemeris_correction(tee_rom_u)
}

sidereal_from_moment <- function(tee) {
  u <- (tee - J2000) / 36525
  poly(u, deg(c(280.46061837, 36525 * 360.98564736629, 0.000387933, -1 / 38710000))) %% 360
}


ephemeris_correction <- function(tee) {
  year <- gregorian_year_from_fixed(floor(tee))
  u <- (gregorian(year, JULY, 1) - gregorian(1900, JANUARY, 1)) / 36525

  case1 <- (2051 <= year & year <= 2150)
  case2 <- (2006 <= year & year <= 2050)
  case3 <- (1987 <= year & year <= 2005)
  case4 <- (1900 <= year & year <= 1986)
  case5 <- (1800 <= year & year <= 1899)
  case6 <- (1700 <= year & year <= 1799)
  case7 <- (1600 <= year & year <= 1699)
  case8 <- (500 <= year & year <= 1599)
  case9 <- (-500 < year & year < 500)

  y1820 <- (year - 1820) / 100
  result <- (1 / 86400) * poly(y1820, c(-20, 0, 32))
  if (any(case1)) {
    result[case1] <- (1 / 86400) * (-20 + 32 * ((year[case1] - 1820) / 100)^2 + 0.5628 * (2150 - year[case1]))
  }
  if (any(case2)) {
    y2000 <- year - 2000
    result[case2] <- (1 / 86400) * poly(y2000[case2], c(62.92, 0.32217, 0.005589))
  }
  if (any(case3)) {
    y2000 <- year - 2000
    result[case3] <- (1 / 86400) * poly(y2000[case3], c(63.86, 0.3345, -0.060374, 0.0017275, 0.000651814, 0.00002373599))
  }
  if (any(case4)) {
    result[case4] <- poly(u[case4], c(-0.00002, 0.000297, 0.025184, -0.181133, 0.553040, -0.861938, 0.677066, -0.212591))
  }
  if (any(case5)) {
    result[case5] <- poly(u[case5], c(-0.000009, 0.003844, 0.083563, 0.865736, 4.867575, 15.845535, 31.332267, 38.291999, 28.316289, 11.636204, 2.043794))
  }
  if (any(case6)) {
    y1700 <- year - 1700
    result[case6] <- (1 / 86400) * poly(y1700[case6], c(8.118780842, -0.005092142, 0.003336121, -0.0000266484))
  }
  if (any(case7)) {
    y1600 <- year - 1600
    result[case7] <- (1 / 86400) * poly(y1600[case7], c(120, -0.9808, -0.01532, 0.000140272128))
  }
  if (any(case8)) {
    y1000 <- (year - 1000) / 100
    result[case8] <- (1 / 86400) * poly(y1000[case8], c(1574.2, -556.01, 71.23472, 0.319781, -0.8503463, -0.005050998, 0.0083572073))
  }
  if (any(case9)) {
    y0 <- year / 100
    result[case9] <- (1 / 86400) * poly(y0[case9], c(10583.6, -1014.41, 33.78311, -5.952053, -0.1798452, 0.022174192, 0.0090316521))
  }
  return(result)
}

equation_of_time <- function(tee) {
  u <- julian_centuries(tee)
  lambda <- poly(u, deg(c(280.46645, 36000.76983, 0.0003032)))
  anomaly <- poly(u, deg(c(357.52910, 35999.05030, -0.0001559, -0.00000048)))
  eccentricity <- poly(u, c(0.016708617, -0.000042037, -0.0000001236))
  varepsilon <- obliquity(tee)
  y <- tan_degrees(varepsilon / 2)^2

  equation <- (1 / (2 * pi)) * (
    y * sin_degrees(2 * lambda) +
      -2 * eccentricity * sin_degrees(anomaly) +
      4 * eccentricity * y * sin_degrees(anomaly) * cos_degrees(2 * lambda) +
      -0.5 * y * y * sin_degrees(4 * lambda) +
      -1.25 * eccentricity * eccentricity * sin_degrees(2 * anomaly)
  )

  sign(equation) * pmin(abs(equation), hr(12))
}

solar_longitude <- function(tee) {
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

  # Calculate the sum
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
  u <- julian_centuries(tee)
  cap_A <- poly(u, deg(c(124.90, -1934.134, 0.002063)))
  cap_B <- poly(u, deg(c(201.11, 72001.5377, 0.00057)))

  deg(-0.004778) * sin_degrees(cap_A) + deg(-0.0003667) * sin_degrees(cap_B)
}

aberration <- function(tee) {
  u <- julian_centuries(tee)

  -deg(0.005575) + deg(0.0000974) * cos_degrees(deg(177.63) + deg(35999.01848) * u)
}

solar_longitude_after <- function(lambda, tee) {
  rate <- MEAN_TROPICAL_YEAR / 360 # Mean days for 1 degree change
  tau <- tee + rate * ((lambda - solar_longitude(tee)) %% 360) # Estimate within 5 days
  a <- pmax(tee, tau - 5) # At or after tee
  b <- tau + 5

  invert_angular(solar_longitude, lambda, c(a, b))
}

season_in_gregorian <- function(season, g_year) {
  jan1 <- gregorian_new_year(g_year)
  solar_longitude_after(season, jan1)
}

# Lunar functions
mean_lunar_longitude <- function(u) {
  poly(u, deg(c(218.3164477, 481267.88123421, -0.0015786, 1 / 538841, -1 / 65194000))) %% 360
}

lunar_elongation <- function(u) {
  poly(u, deg(c(297.8501921, 445267.1114034, -0.0018819, 1 / 545868, -1 / 113065000))) %% 360
}

solar_anomaly <- function(u) {
  poly(u, deg(c(357.5291092, 35999.0502909, -0.0001536, 1 / 24490000))) %% 360
}

lunar_anomaly <- function(u) {
  poly(u, deg(c(134.9633964, 477198.8675055, 0.0087414, 1 / 69699, -1 / 14712000))) %% 360
}

moon_node <- function(u) {
  poly(u, deg(c(93.2720950, 483202.0175233, -0.0036539, -1 / 3526000, 1 / 863310000))) %% 360
}

lunar_node <- function(date) {
  moon_node(julian_centuries(date)) %% 360
}

lunar_longitude <- function(tee) {
  u <- julian_centuries(tee)
  cap_L_prime <- mean_lunar_longitude(u)
  cap_D <- lunar_elongation(u)
  cap_M <- solar_anomaly(u)
  cap_M_prime <- lunar_anomaly(u)
  cap_F <- moon_node(u)
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

  venus <- deg(3958 / 1000000) * sin_degrees(deg(119.75) + u * deg(131.849))
  jupiter <- deg(318 / 1000000) * sin_degrees(deg(53.09) + u * deg(479264.29))
  flat_earth <- deg(1962 / 1000000) * sin_degrees(cap_L_prime - cap_F)

  (cap_L_prime + correction + venus + jupiter + flat_earth + nutation(tee)) %% 360
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

nth_new_moon <- function(n) {
  n0 <- 24724 # Months from RD 0 until j2000
  k <- n - n0 # Months since j2000
  u <- k / 1236.85 # Julian centuries

  approx <- J2000 + poly(u, c(5.09766, MEAN_SYNODIC_MONTH * 1236.85, 0.00015437, -0.00000015, 0.00000000073))
  cap_E <- poly(u, c(1, -0.002516, -0.0000074))
  solar_anomaly <- poly(u, deg(c(2.5534, 1236.85 * 29.10535670, -0.0000014, -0.00000011)))
  lunar_anomaly <- poly(u, deg(c(201.5643, 385.81693528 * 1236.85, 0.0107582, 0.00001238, -0.000000058)))
  moon_argument <- poly(u, deg(c(160.7108, 390.67050284 * 1236.85, -0.0016118, -0.00000227, 0.000000011)))
  cap_omega <- poly(u, deg(c(124.7746, -1.56375588 * 1236.85, 0.0020672, 0.00000215)))

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
  t0 <- nth_new_moon(0)
  phi <- lunar_phase(tee)
  n <- round((tee - t0) / MEAN_SYNODIC_MONTH - phi / 360)

  final_value(n - 1, function(k) nth_new_moon(k) < tee)
}

new_moon_at_or_after <- function(tee) {
  t0 <- nth_new_moon(0)
  phi <- lunar_phase(tee)
  n <- round((tee - t0) / MEAN_SYNODIC_MONTH - phi / 360)

  next_value(n, function(k) nth_new_moon(k) >= tee)
}

lunar_phase_at_or_before <- function(phi, tee) {
  tau <- tee - MEAN_SYNODIC_MONTH * ((lunar_phase(tee) - phi) %% 360) / 360
  a <- tau - 2
  b <- pmin(tee, tau + 2)

  invert_angular(lunar_phase, phi, c(a, b))
}

lunar_phase_at_or_after <- function(phi, tee) {
  tau <- tee + MEAN_SYNODIC_MONTH * ((phi - lunar_phase(tee)) %% 360) / 360
  a <- pmax(tee, tau - 2)
  b <- tau + 2

  invert_angular(lunar_phase, phi, c(a, b))
}
