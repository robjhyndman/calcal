#==============================================================================
# Solar and Lunar Functions
#==============================================================================

universal_from_dynamical <- function(tee) {
  tee - ephemeris_correction(tee)
}

dynamical_from_universal <- function(tee_rom_u) {
  tee_rom_u + ephemeris_correction(tee_rom_u)
}

sidereal_from_moment <- function(tee) {
  c <- (tee - J2000) / 36525
  poly(c, deg(c(280.46061837, 36525 * 360.98564736629, 0.000387933, -1/38710000))) %% 360
}

MEAN_TROPICAL_YEAR <- 365.242189
MEAN_SIDEREAL_YEAR <- 365.25636
MEAN_SYNODIC_MONTH <- 29.530588861

ephemeris_correction <- function(tee) {
  year <- gregorian_year_from_fixed(floor(tee))
  c <- gregorian_date_difference(
    gregorian_date(1900, JANUARY, 1),
    gregorian_date(year, JULY, 1)
  ) / 36525
  
  if (2051 <= year && year <= 2150) {
    (1/86400) * (-20 + 32 * ((year - 1820) / 100)^2 + 0.5628 * (2150 - year))
  } else if (2006 <= year && year <= 2050) {
    y2000 <- year - 2000
    (1/86400) * poly(y2000, c(62.92, 0.32217, 0.005589))
  } else if (1987 <= year && year <= 2005) {
    y2000 <- year - 2000
    (1/86400) * poly(y2000, c(63.86, 0.3345, -0.060374, 0.0017275, 0.000651814, 0.00002373599))
  } else if (1900 <= year && year <= 1986) {
    poly(c, c(-0.00002, 0.000297, 0.025184, -0.181133, 0.553040, -0.861938, 0.677066, -0.212591))
  } else if (1800 <= year && year <= 1899) {
    poly(c, c(-0.000009, 0.003844, 0.083563, 0.865736, 4.867575, 15.845535, 31.332267, 38.291999, 28.316289, 11.636204, 2.043794))
  } else if (1700 <= year && year <= 1799) {
    y1700 <- year - 1700
    (1/86400) * poly(y1700, c(8.118780842, -0.005092142, 0.003336121, -0.0000266484))
  } else if (1600 <= year && year <= 1699) {
    y1600 <- year - 1600
    (1/86400) * poly(y1600, c(120, -0.9808, -0.01532, 0.000140272128))
  } else if (500 <= year && year <= 1599) {
    y1000 <- (year - 1000) / 100
    (1/86400) * poly(y1000, c(1574.2, -556.01, 71.23472, 0.319781, -0.8503463, -0.005050998, 0.0083572073))
  } else if (-500 < year && year < 500) {
    y0 <- year / 100
    (1/86400) * poly(y0, c(10583.6, -1014.41, 33.78311, -5.952053, -0.1798452, 0.022174192, 0.0090316521))
  } else {
    y1820 <- (year - 1820) / 100
    (1/86400) * poly(y1820, c(-20, 0, 32))
  }
}

equation_of_time <- function(tee) {
  c <- julian_centuries(tee)
  lambda <- poly(c, deg(c(280.46645, 36000.76983, 0.0003032)))
  anomaly <- poly(c, deg(c(357.52910, 35999.05030, -0.0001559, -0.00000048)))
  eccentricity <- poly(c, c(0.016708617, -0.000042037, -0.0000001236))
  varepsilon <- obliquity(tee)
  y <- tan_degrees(varepsilon / 2)^2
  
  equation <- (1 / (2 * pi)) * (
    y * sin_degrees(2 * lambda) +
    -2 * eccentricity * sin_degrees(anomaly) +
    4 * eccentricity * y * sin_degrees(anomaly) * cos_degrees(2 * lambda) +
    -0.5 * y * y * sin_degrees(4 * lambda) +
    -1.25 * eccentricity * eccentricity * sin_degrees(2 * anomaly)
  )
  
  sign(equation) * min(abs(equation), hr(12))
}

solar_longitude <- function(tee) {
  c <- julian_centuries(tee)
  
  # Simplified calculation
  lambda <- deg(282.7771834) + 
            deg(36000.76953744) * c +
            deg(0.000005729577951308232) * 
            sum(c(403406, 195207, 119433, 112392, 3891, 2819, 1721, 660, 350, 334, 314, 268, 242, 234, 
                  158, 132, 129, 114, 99, 93, 86, 78, 72, 68, 64, 46, 38, 37, 32, 29, 28, 27, 25, 
                  24, 21, 21, 20, 18, 17, 14, 13, 13, 13, 12, 10, 10, 10, 10) *
                sin_degrees(c(270.54861, 340.19128, 63.91854, 331.26220, 317.843, 86.631, 240.052, 
                             310.26, 247.23, 260.87, 297.82, 343.14, 166.79, 81.53, 3.50, 132.75, 
                             182.95, 162.03, 29.8, 266.4, 249.2, 157.6, 257.8, 185.1, 69.9, 8.0, 
                             197.1, 250.4, 65.3, 162.7, 341.5, 291.6, 98.5, 146.7, 110.0, 5.2, 
                             342.6, 230.9, 256.1, 45.3, 242.9, 115.2, 151.8, 285.3, 53.3, 126.6, 
                             205.7, 85.9, 146.1) +
                         c(0.9287892, 35999.1376958, 35999.4089666, 35998.7287385, 71998.20261, 
                           71998.4403, 36000.35726, 71997.4812, 32964.4678, -19.4410, 445267.1117, 
                           45036.8840, 3.1008, 22518.4434, -19.9739, 65928.9345, 9038.0293, 
                           3034.7684, 33718.148, 3034.448, -2280.773, 29929.992, 31556.493, 
                           149.588, 9037.750, 107997.405, -4444.176, 151.771, 67555.316, 31556.080, 
                           -4561.540, 107996.706, 1221.655, 62894.167, 31437.369, 14578.298, 
                           -31931.757, 34777.243, 1221.999, 62894.511, -4442.039, 107997.909, 
                           119.066, 16859.071, -4.578, 26895.292, -39.127, 12297.536, 90073.778) * c))
  
  (lambda + aberration(tee) + nutation(tee)) %% 360
}

nutation <- function(tee) {
  c <- julian_centuries(tee)
  cap_A <- poly(c, deg(c(124.90, -1934.134, 0.002063)))
  cap_B <- poly(c, deg(c(201.11, 72001.5377, 0.00057)))
  
  deg(-0.004778) * sin_degrees(cap_A) + deg(-0.0003667) * sin_degrees(cap_B)
}

aberration <- function(tee) {
  c <- julian_centuries(tee)
  
  -deg(0.005575) + deg(0.0000974) * cos_degrees(deg(177.63) + deg(35999.01848) * c)
}

solar_longitude_after <- function(lambda, tee) {
  rate <- MEAN_TROPICAL_YEAR / 360  # Mean days for 1 degree change
  tau <- tee + rate * ((lambda - solar_longitude(tee)) %% 360)  # Estimate within 5 days
  a <- max(tee, tau - 5)  # At or after tee
  b <- tau + 5
  
  invert_angular(solar_longitude, lambda, c(a, b))
}

SPRING <- deg(0)
SUMMER <- deg(90)
AUTUMN <- deg(180)
WINTER <- deg(270)

season_in_gregorian <- function(season, g_year) {
  jan1 <- gregorian_new_year(g_year)
  solar_longitude_after(season, jan1)
}

# Lunar functions
mean_lunar_longitude <- function(c) {
  poly(c, deg(c(218.3164477, 481267.88123421, -0.0015786, 1/538841, -1/65194000))) %% 360
}

lunar_elongation <- function(c) {
  poly(c, deg(c(297.8501921, 445267.1114034, -0.0018819, 1/545868, -1/113065000))) %% 360
}

solar_anomaly <- function(c) {
  poly(c, deg(c(357.5291092, 35999.0502909, -0.0001536, 1/24490000))) %% 360
}

lunar_anomaly <- function(c) {
  poly(c, deg(c(134.9633964, 477198.8675055, 0.0087414, 1/69699, -1/14712000))) %% 360
}

moon_node <- function(c) {
  poly(c, deg(c(93.2720950, 483202.0175233, -0.0036539, -1/3526000, 1/863310000))) %% 360
}

lunar_node <- function(date) {
  moon_node(julian_centuries(date)) %% 360
}

lunar_longitude <- function(tee) {
  c <- julian_centuries(tee)
  cap_L_prime <- mean_lunar_longitude(c)
  cap_D <- lunar_elongation(c)
  cap_M <- solar_anomaly(c)
  cap_M_prime <- lunar_anomaly(c)
  cap_F <- moon_node(c)
  cap_E <- poly(c, c(1, -0.002516, -0.0000074))
  
  # Simplified calculation
  correction <- deg(1/1000000) * 
    sum(c(6288774, 1274027, 658314, 213618, -185116, -114332, 58793, 57066, 53322, 45758, 
          -40923, -34720, -30383, 15327, -12528, 10980, 10675, 10034, 8548, -7888, -6766, 
          -5163, 4987, 4036, 3994, 3861, 3665, -2689, -2602, 2390, -2348, 2236, -2120, 
          -2069, 2048, -1773, -1595, 1215, -1110, -892, -810, 759, -713, -700, 691, 596, 
          549, 537, 520, -487, -399, -381, 351, -340, 330, 327, -323, 299, 294) *
        (cap_E^(abs(c(0, 0, 0, 0, 1, 0, 0, -1, 0, -1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 
                       1, -1, 0, 0, 0, 1, 0, -1, 0, -2, 1, 2, -2, 0, 0, -1, 0, 0, 1, -1, 2, 
                       2, 1, -1, 0, 0, -1, 0, 1, 0, 1, 0, 0, -1, 2, 1, 0)))) *
        sin_degrees(c(0, 2, 2, 0, 0, 0, 2, 2, 2, 2, 0, 1, 0, 2, 0, 0, 4, 0, 4, 2, 2, 1, 1, 
                      2, 2, 4, 2, 0, 2, 2, 1, 2, 0, 0, 2, 2, 2, 4, 0, 3, 2, 4, 0, 2, 2, 2, 
                      4, 0, 4, 1, 2, 0, 1, 3, 4, 2, 0, 1, 2) * cap_D +
                      c(0, 0, 0, 0, 1, 0, 0, -1, 0, -1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 
                        1, -1, 0, 0, 0, 1, 0, -1, 0, -2, 1, 2, -2, 0, 0, -1, 0, 0, 1, -1, 2, 
                        2, 1, -1, 0, 0, -1, 0, 1, 0, 1, 0, 0, -1, 2, 1, 0) * cap_M +
                      c(1, -1, 0, 2, 0, 0, -2, -1, 1, 0, -1, 0, 1, 0, 1, 1, -1, 3, -2, -1, 
                        0, -1, 0, 1, 2, 0, -3, -2, -1, -2, 1, 0, 2, 0, -1, 1, 0, -1, 2, -1, 
                        1, -2, -1, -1, -2, 0, 1, 4, 0, -2, 0, 2, 1, -2, -3, 2, 1, -1, 3) * cap_M_prime +
                      c(0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, -2, 2, -2, 0, 0, 0, 0, 0, 0, 
                        0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, -2, 2, 0, 2, 0, 0, 0, 0, 0, 
                        0, -2, 0, 0, 0, 0, -2, -2, 0, 0, 0, 0, 0, 0, 0) * cap_F))
  
  venus <- deg(3958/1000000) * sin_degrees(deg(119.75) + c * deg(131.849))
  jupiter <- deg(318/1000000) * sin_degrees(deg(53.09) + c * deg(479264.29))
  flat_earth <- deg(1962/1000000) * sin_degrees(cap_L_prime - cap_F)
  
  (cap_L_prime + correction + venus + jupiter + flat_earth + nutation(tee)) %% 360
}

lunar_phase <- function(tee) {
  phi <- (lunar_longitude(tee) - solar_longitude(tee)) %% 360
  t0 <- nth_new_moon(0)
  n <- round((tee - t0) / MEAN_SYNODIC_MONTH)
  phi_prime <- 360 * ((tee - nth_new_moon(n)) / MEAN_SYNODIC_MONTH) %% 1
  
  if (abs(phi - phi_prime) > 180) phi_prime else phi
}

nth_new_moon <- function(n) {
  n0 <- 24724  # Months from RD 0 until j2000
  k <- n - n0  # Months since j2000
  c <- k / 1236.85  # Julian centuries
  
  approx <- J2000 + poly(c, c(5.09766, MEAN_SYNODIC_MONTH * 1236.85, 0.00015437, -0.00000015, 0.00000000073))
  cap_E <- poly(c, c(1, -0.002516, -0.0000074))
  solar_anomaly <- poly(c, deg(c(2.5534, 1236.85 * 29.10535670, -0.0000014, -0.00000011)))
  lunar_anomaly <- poly(c, deg(c(201.5643, 385.81693528 * 1236.85, 0.0107582, 0.00001238, -0.000000058)))
  moon_argument <- poly(c, deg(c(160.7108, 390.67050284 * 1236.85, -0.0016118, -0.00000227, 0.000000011)))
  cap_omega <- poly(c, deg(c(124.7746, -1.56375588 * 1236.85, 0.0020672, 0.00000215)))
  
  correction <- -0.00017 * sin_degrees(cap_omega) +
    sum(c(-0.40720, 0.17241, 0.01608, 0.01039, 0.00739, -0.00514, 0.00208, -0.00111, 
          -0.00057, 0.00056, -0.00042, 0.00042, 0.00038, -0.00024, -0.00007, 0.00004, 
          0.00004, 0.00003, 0.00003, -0.00003, 0.00003, -0.00002, -0.00002, 0.00002) *
        (cap_E^(c(0, 1, 0, 0, -1, 1, 2, 0, 0, 1, 0, 1, 1, -1, 2, 0, 3, 1, 0, 1, -1, -1, 1, 0))) *
        sin_degrees(c(0, 1, 0, 0, -1, 1, 2, 0, 0, 1, 0, 1, 1, -1, 2, 0, 3, 1, 0, 1, -1, -1, 1, 0) * solar_anomaly +
                   c(1, 0, 2, 0, 1, 1, 0, 1, 1, 2, 3, 0, 0, 2, 1, 2, 0, 1, 2, 1, 1, 1, 3, 4) * lunar_anomaly +
                   c(0, 0, 0, 2, 0, 0, 0, -2, 2, 0, 0, 2, -2, 0, 0, -2, 0, -2, 2, 2, 2, -2, 0, 0) * moon_argument))
  
  extra <- 0.000325 * sin_degrees(poly(c, deg(c(299.77, 132.8475848, -0.009173))))
  
  additional <- sum(c(0.000165, 0.000164, 0.000126, 0.000110, 0.000062, 0.000060, 0.000056, 
                     0.000047, 0.000042, 0.000040, 0.000037, 0.000035, 0.000023) *
                   sin_degrees(c(251.88, 251.83, 349.42, 84.66, 141.74, 207.14, 154.84, 34.52, 
                                207.19, 291.34, 161.72, 239.56, 331.55) +
                              c(0.016321, 26.651886, 36.412478, 18.206239, 53.303771, 2.453732, 
                                7.306860, 27.261239, 0.121824, 1.844379, 24.198154, 25.513099, 
                                3.592518) * k))
  
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
  b <- min(tee, tau + 2)
  
  invert_angular(lunar_phase, phi, c(a, b))
}

NEW <- deg(0)
FIRST_QUARTER <- deg(90)
FULL <- deg(180)
LAST_QUARTER <- deg(270)

lunar_phase_at_or_after <- function(phi, tee) {
  tau <- tee + MEAN_SYNODIC_MONTH * ((phi - lunar_phase(tee)) %% 360) / 360
  a <- max(tee, tau - 2)
  b <- tau + 2
  
  invert_angular(lunar_phase, phi, c(a, b))
}
