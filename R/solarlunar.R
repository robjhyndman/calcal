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
  poly(
    u,
    deg(c(280.46061837, 36525 * 360.98564736629, 0.000387933, -1 / 38710000))
  ) %%
    360
}

ephemeris_correction <- function(tee) {
  year <- gregorian_year_from_fixed(floor(tee))
  u <- (gregorian_date(year, JULY, 1) - gregorian_date(1900, JANUARY, 1)) /
    36525

  case1 <- (2051 <= year & year <= 2150) & !is.na(year)
  case2 <- (2006 <= year & year <= 2050) & !is.na(year)
  case3 <- (1987 <= year & year <= 2005) & !is.na(year)
  case4 <- (1900 <= year & year <= 1986) & !is.na(year)
  case5 <- (1800 <= year & year <= 1899) & !is.na(year)
  case6 <- (1700 <= year & year <= 1799) & !is.na(year)
  case7 <- (1600 <= year & year <= 1699) & !is.na(year)
  case8 <- (500 <= year & year <= 1599) & !is.na(year)
  case9 <- (-500 < year & year < 500) & !is.na(year)

  y1820 <- (year - 1820) / 100
  result <- (1 / 86400) * poly(y1820, c(-20, 0, 32))
  if (any(case1)) {
    result[case1] <- (1 / 86400) *
      (-20 +
        32 * ((year[case1] - 1820) / 100)^2 +
        0.5628 * (2150 - year[case1]))
  }
  if (any(case2)) {
    y2000 <- year - 2000
    result[case2] <- (1 / 86400) *
      poly(y2000[case2], c(62.92, 0.32217, 0.005589))
  }
  if (any(case3)) {
    y2000 <- year - 2000
    result[case3] <- (1 / 86400) *
      poly(
        y2000[case3],
        c(63.86, 0.3345, -0.060374, 0.0017275, 0.000651814, 0.00002373599)
      )
  }
  if (any(case4)) {
    result[case4] <- poly(
      u[case4],
      c(
        -0.00002,
        0.000297,
        0.025184,
        -0.181133,
        0.553040,
        -0.861938,
        0.677066,
        -0.212591
      )
    )
  }
  if (any(case5)) {
    result[case5] <- poly(
      u[case5],
      c(
        -0.000009,
        0.003844,
        0.083563,
        0.865736,
        4.867575,
        15.845535,
        31.332267,
        38.291999,
        28.316289,
        11.636204,
        2.043794
      )
    )
  }
  if (any(case6)) {
    y1700 <- year - 1700
    result[case6] <- (1 / 86400) *
      poly(
        y1700[case6],
        c(8.118780842, -0.005092142, 0.003336121, -0.0000266484)
      )
  }
  if (any(case7)) {
    y1600 <- year - 1600
    result[case7] <- (1 / 86400) *
      poly(y1600[case7], c(120, -0.9808, -0.01532, 0.000140272128))
  }
  if (any(case8)) {
    y1000 <- (year - 1000) / 100
    result[case8] <- (1 / 86400) *
      poly(
        y1000[case8],
        c(
          1574.2,
          -556.01,
          71.23472,
          0.319781,
          -0.8503463,
          -0.005050998,
          0.0083572073
        )
      )
  }
  if (any(case9)) {
    y0 <- year / 100
    result[case9] <- (1 / 86400) *
      poly(
        y0[case9],
        c(
          10583.6,
          -1014.41,
          33.78311,
          -5.952053,
          -0.1798452,
          0.022174192,
          0.0090316521
        )
      )
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

  equation <- (1 / (2 * pi)) *
    (y *
      sin_degrees(2 * lambda) +
      -2 * eccentricity * sin_degrees(anomaly) +
      4 * eccentricity * y * sin_degrees(anomaly) * cos_degrees(2 * lambda) +
      -0.5 * y * y * sin_degrees(4 * lambda) +
      -1.25 * eccentricity * eccentricity * sin_degrees(2 * anomaly))

  sign(equation) * pmin(abs(equation), hr(12))
}

solar_longitude <- function(tee) {
  u <- julian_centuries(tee)

  # Calculate the sum
  parts <- matrix(
    0,
    nrow = length(SOLAR_LONGITUDE_COEFFS$coefficients),
    ncol = length(u)
  )
  for (i in seq_along(SOLAR_LONGITUDE_COEFFS$coefficients)) {
    parts[i, ] <- SOLAR_LONGITUDE_COEFFS$coefficients[i] *
      sin_degrees(
        SOLAR_LONGITUDE_COEFFS$addends[i] +
          SOLAR_LONGITUDE_COEFFS$multipliers[i] * u
      )
  }

  longitude <- deg(282.7771834) +
    36000.76953744 * u +
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

  -deg(0.005575) +
    deg(0.0000974) * cos_degrees(deg(177.63) + deg(35999.01848) * u)
}

solar_longitude_after <- function(lambda, tee) {
  rate <- MEAN_TROPICAL_YEAR / 360 # Mean days for 1 degree change
  tau <- tee + rate * ((lambda - solar_longitude(tee)) %% 360) # Estimate within 5 days
  a <- pmax(tee, tau - 5) # At or after tee
  b <- tau + 5

  invert_angular(solar_longitude, lambda, a, b)
}

season_in_gregorian <- function(season, g_year) {
  jan1 <- gregorian_new_year(g_year)
  solar_longitude_after(season, jan1)
}

estimate_prior_solar_longitude <- function(lambda, tee) {
  # TYPE (season moment) -> moment
  # Approximate moment at or before tee
  # when solar longitude just exceeded lambda degrees.

  # Mean change of one degree
  rate <- MEAN_TROPICAL_YEAR / deg(360)

  # First approximation
  tau <- tee - (rate * ((solar_longitude(tee) - lambda) %% 360))

  # Difference in longitude
  cap_Delta <- mod3(solar_longitude(tau) - lambda, -180, 180)

  # Return minimum of tee and adjusted tau
  pmin(tee, tau - (rate * cap_Delta))
}

# Lunar functions
mean_lunar_longitude <- function(u) {
  poly(
    u,
    deg(c(218.3164477, 481267.88123421, -0.0015786, 1 / 538841, -1 / 65194000))
  ) %%
    360
}

lunar_elongation <- function(u) {
  poly(
    u,
    deg(c(297.8501921, 445267.1114034, -0.0018819, 1 / 545868, -1 / 113065000))
  ) %%
    360
}

solar_anomaly <- function(u) {
  poly(u, deg(c(357.5291092, 35999.0502909, -0.0001536, 1 / 24490000))) %% 360
}

lunar_anomaly <- function(u) {
  poly(
    u,
    deg(c(134.9633964, 477198.8675055, 0.0087414, 1 / 69699, -1 / 14712000))
  ) %%
    360
}

moon_node <- function(u) {
  poly(
    u,
    deg(c(93.2720950, 483202.0175233, -0.0036539, -1 / 3526000, 1 / 863310000))
  ) %%
    360
}

lunar_node <- function(date) {
  moon_node(julian_centuries(date)) |> mod3(-90, 90)
}

lunar_longitude <- function(tee) {
  u <- julian_centuries(tee)
  cap_L_prime <- mean_lunar_longitude(u)
  cap_D <- lunar_elongation(u)
  cap_M <- solar_anomaly(u)
  cap_M_prime <- lunar_anomaly(u)
  cap_F <- moon_node(u)
  cap_E <- poly(u, c(1, -0.002516, -0.0000074))

  parts <- matrix(
    0,
    nrow = length(LUNAR_LONGITUDE_COEFFS$sine_coefficients),
    ncol = length(u)
  )
  for (i in seq_along(LUNAR_LONGITUDE_COEFFS$sine_coefficients)) {
    parts[i, ] <- LUNAR_LONGITUDE_COEFFS$sine_coefficients[i] *
      (cap_E^abs(LUNAR_LONGITUDE_COEFFS$args_solar_anomaly[i])) *
      sin_degrees(
        (LUNAR_LONGITUDE_COEFFS$args_lunar_elongation[i] * cap_D) +
          (LUNAR_LONGITUDE_COEFFS$args_solar_anomaly[i] * cap_M) +
          (LUNAR_LONGITUDE_COEFFS$args_lunar_anomaly[i] * cap_M_prime) +
          (LUNAR_LONGITUDE_COEFFS$args_moon_node[i] * cap_F)
      )
  }
  correction <- (deg(1 / 1000000)) * colSums(parts)

  venus <- deg(3958 / 1000000) * sin_degrees(deg(119.75) + u * deg(131.849))
  jupiter <- deg(318 / 1000000) * sin_degrees(deg(53.09) + u * deg(479264.29))
  flat_earth <- deg(1962 / 1000000) * sin_degrees(cap_L_prime - cap_F)

  (cap_L_prime + correction + venus + jupiter + flat_earth + nutation(tee)) %%
    360
}

#' Lunar phase at date
#'
#' Lunar phase at date, as an angle in degrees. An angle of 0 means a new moon,
#' 90 degrees means the first quarter, 180 means a full moon, and 270 degrees
#' means the last quarter.
#'
#' @param date Date vector
#' @examples
#' april2025 <- gregorian_date(2025, 4, 1:30)
#' lunar_phase(april2025)
#'
#' @export
lunar_phase <- function(date) {
  tee <- vec_data(date)
  phi <- (lunar_longitude(tee) - solar_longitude(tee)) %% 360
  t0 <- nth_new_moon(0)
  n <- round((tee - t0) / MEAN_SYNODIC_MONTH)
  phi_prime <- 360 * ((tee - nth_new_moon(n)) / MEAN_SYNODIC_MONTH) %% 1
  idx <- which(abs(phi - phi_prime) > 180)
  phi[idx] <- phi_prime[idx]
  return(phi)
}

nth_new_moon <- function(n) {
  n0 <- 24724 # Months from RD 0 until j2000
  k <- n - n0 # Months since j2000
  u <- k / 1236.85 # Julian centuries

  approx <- J2000 +
    poly(
      u,
      c(
        5.09766,
        MEAN_SYNODIC_MONTH * 1236.85,
        0.00015437,
        -0.00000015,
        0.00000000073
      )
    )
  cap_E <- poly(u, c(1, -0.002516, -0.0000074))
  solar_anomaly <- poly(
    u,
    deg(c(2.5534, 1236.85 * 29.10535670, -0.0000014, -0.00000011))
  )
  lunar_anomaly <- poly(
    u,
    deg(c(
      201.5643,
      385.81693528 * 1236.85,
      0.0107582,
      0.00001238,
      -0.000000058
    ))
  )
  moon_argument <- poly(
    u,
    deg(c(
      160.7108,
      390.67050284 * 1236.85,
      -0.0016118,
      -0.00000227,
      0.000000011
    ))
  )
  cap_omega <- poly(
    u,
    deg(c(124.7746, -1.56375588 * 1236.85, 0.0020672, 0.00000215))
  )

  parts <- matrix(
    0,
    nrow = length(NTH_NEW_MOON_COEFFS$sine_coeff),
    ncol = length(u)
  )
  for (i in seq_along(NTH_NEW_MOON_COEFFS$sine_coeff)) {
    parts[i, ] <- NTH_NEW_MOON_COEFFS$sine_coeff[i] *
      (cap_E^abs(NTH_NEW_MOON_COEFFS$E_factor[i])) *
      sin_degrees(
        NTH_NEW_MOON_COEFFS$solar_coeff[i] *
          solar_anomaly +
          NTH_NEW_MOON_COEFFS$lunar_coeff[i] * lunar_anomaly +
          NTH_NEW_MOON_COEFFS$moon_coeff[i] * moon_argument
      )
  }
  correction <- (deg(-0.00017) * sin_degrees(cap_omega)) + colSums(parts)

  parts <- matrix(
    0,
    nrow = length(NTH_NEW_MOON_COEFFS$add_const),
    ncol = length(u)
  )
  for (i in seq_along(NTH_NEW_MOON_COEFFS$add_const)) {
    parts[i, ] <- NTH_NEW_MOON_COEFFS$add_factor[i] *
      sin_degrees(
        NTH_NEW_MOON_COEFFS$add_const[i] + NTH_NEW_MOON_COEFFS$add_coeff[i] * k
      )
  }
  additional <- colSums(parts)

  extra <- deg(0.000325) *
    sin_degrees(poly(u, deg(c(299.77, 132.8475848, -0.009173))))

  universal_from_dynamical(approx + correction + extra + additional)
}

new_moon_before <- function(tee) {
  first <- get_new_moon_at_or_after(min(tee) - 30)
  last <- get_new_moon_before(max(tee) + 30)
  j <- round(first:last)
  j[findInterval(tee, nth_new_moon(j))]
}

get_new_moon_before <- function(tee) {
  t0 <- nth_new_moon(0)
  phi <- lunar_phase(tee)
  n <- round((tee - t0) / MEAN_SYNODIC_MONTH - phi / 360)
  final_value(n - 1, function(k) {
    nth_new_moon(k) < tee
  })
}

new_moon_at_or_after <- function(tee) {
  first <- get_new_moon_at_or_after(min(tee) - 30)
  last <- get_new_moon_before(max(tee) + 30)
  j <- round(first:last)
  j[findInterval(tee, nth_new_moon(j))] + 1
}

get_new_moon_at_or_after <- function(tee) {
  t0 <- nth_new_moon(0)
  phi <- lunar_phase(tee)
  n <- round((tee - t0) / MEAN_SYNODIC_MONTH - phi / 360)
  next_value(n, function(k) nth_new_moon(k) >= tee)
}

lunar_phase_at_or_before <- function(phi, tee) {
  tau <- tee - MEAN_SYNODIC_MONTH * ((lunar_phase(tee) - phi) %% 360) / 360
  a <- tau - 2
  b <- pmin(tee, tau + 2)

  invert_angular(lunar_phase, phi, a, b)
}

lunar_phase_at_or_after <- function(phi, tee) {
  tau <- tee + MEAN_SYNODIC_MONTH * ((phi - lunar_phase(tee)) %% 360) / 360
  a <- pmax(tee, tau - 2)
  b <- tau + 2

  invert_angular(lunar_phase, phi, a, b)
}

lunar_latitude <- function(tee) {
  # TYPE moment -> half-circle
  # Latitude of moon (in degrees) at moment tee.
  # Adapted from "Astronomical Algorithms" by Jean Meeus,
  # Willmann-Bell, 2nd edn., 1998, pp. 338-342.
  u <- julian_centuries(tee)
  cap_L_prime <- mean_lunar_longitude(u)
  cap_D <- lunar_elongation(u)
  cap_M <- solar_anomaly(u)
  cap_M_prime <- lunar_anomaly(u)
  cap_F <- moon_node(u)
  cap_E <- poly(u, c(1, -0.002516, -0.0000074))

  parts <- matrix(
    0,
    nrow = length(LUNAR_LATITUDE_COEFFS$sine_coeff),
    ncol = length(u)
  )
  for (i in seq_along(LUNAR_LATITUDE_COEFFS$sine_coeff)) {
    parts[i, ] <- LUNAR_LATITUDE_COEFFS$sine_coeff[i] *
      (cap_E^abs(LUNAR_LATITUDE_COEFFS$args_solar_anomaly[i])) *
      sin_degrees(
        (LUNAR_LATITUDE_COEFFS$args_lunar_elongation[i] * cap_D) +
          (LUNAR_LATITUDE_COEFFS$args_solar_anomaly[i] * cap_M) +
          (LUNAR_LATITUDE_COEFFS$args_lunar_anomaly[i] * cap_M_prime) +
          (LUNAR_LATITUDE_COEFFS$args_moon_node[i] * cap_F)
      )
  }

  beta <- deg(1 / 1000000) * colSums(parts)

  venus <- deg(175 / 1000000) *
    (sin_degrees(deg(119.75) + u * deg(131.849) + cap_F) +
      sin_degrees(deg(119.75) + u * deg(131.849) - cap_F))

  flat_earth <- deg(-2235 / 1000000) *
    sin_degrees(cap_L_prime) +
    deg(127 / 1000000) * sin_degrees(cap_L_prime - cap_M_prime) +
    deg(-115 / 1000000) * sin_degrees(cap_L_prime + cap_M_prime)

  extra <- deg(382 / 1000000) * sin_degrees(deg(313.45) + u * deg(481266.484))

  beta + venus + flat_earth + extra
}


# Fully vectorized lunar distance calculation
lunar_distance <- function(tee) {
  tee <- as.vector(tee)
  u <- julian_centuries(tee)

  # Get astronomical parameters
  cap_D <- lunar_elongation(u)
  cap_M <- solar_anomaly(u)
  cap_M_prime <- lunar_anomaly(u)
  cap_F <- moon_node(u)
  cap_E <- 1 - 0.002516 * u - 0.0000074 * u^2

  parts <- matrix(
    0,
    nrow = length(LUNAR_DISTANCE_COEFFS$cosine_coeff),
    ncol = length(u)
  )
  for (i in seq_along(LUNAR_DISTANCE_COEFFS$cosine_coeff)) {
    parts[i, ] <- LUNAR_DISTANCE_COEFFS$cosine_coeff[i] *
      (cap_E^abs(LUNAR_DISTANCE_COEFFS$args_solar_anomaly[i])) *
      cos_degrees(
        (LUNAR_DISTANCE_COEFFS$args_lunar_elongation[i] * cap_D) +
          (LUNAR_DISTANCE_COEFFS$args_solar_anomaly[i] * cap_M) +
          (LUNAR_DISTANCE_COEFFS$args_lunar_anomaly[i] * cap_M_prime) +
          (LUNAR_DISTANCE_COEFFS$args_moon_node[i] * cap_F)
      )
  }

  correction <- colSums(parts)

  mt(385000560) + correction
}

lunar_altitude <- function(tee, location) {
  # Geocentric altitude of moon at tee at location,
  # as a small positive/negative angle in degrees, ignoring
  # parallax and refraction. Adapted from "Astronomical
  # Algorithms" by Jean Meeus, Willmann-Bell, 2nd edn., 1998.
  phi <- latitude(location) # Local latitude.
  psi <- longitude(location) # Local longitude.
  lambda <- lunar_longitude(tee) # Lunar longitude.
  beta <- lunar_latitude(tee) # Lunar latitude.
  alpha <- right_ascension(tee, beta, lambda) # Lunar right ascension.
  delta <- declination(tee, beta, lambda) # Lunar declination.
  theta0 <- sidereal_from_moment(tee) # Sidereal time.
  cap_H <- (theta0 + psi - alpha) %% 360 # Local hour angle.

  altitude <- arcsin_degrees(
    sin_degrees(phi) *
      sin_degrees(delta) +
      cos_degrees(phi) * cos_degrees(delta) * cos_degrees(cap_H)
  )

  mod3(altitude, -180, 180)
}

lunar_parallax <- function(tee, location) {
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
  lunar_altitude(tee, location) - lunar_parallax(tee, location)
}

lunar_diameter <- function(tee) {
  deg(1792367000 / 9) / lunar_distance(tee)
}

observed_lunar_altitude <- function(tee, location) {
  topocentric_lunar_altitude(tee, location) +
    refraction(tee, location) +
    mins(16)
}


#' @rdname sunrise
#' @export
moonset <- function(date, location) {
  if (!inherits(date, "rdvec")) {
    # Convert to some calendar
    date <- as_gregorian(date)
  }
  date <- vec_data(date)
  lst <- vec_recycle_common(
    date = date,
    location = location,
    .size = max(length(date), length(location))
  )
  tee <- universal_from_standard(lst$date, lst$location) # Midnight.
  waxing <- lunar_phase(tee) < deg(180)
  alt <- observed_lunar_altitude(tee, lst$location) # Altitude at midnight.
  lat <- latitude(lst$location)
  offset <- alt / (4 * (deg(90) - abs(lat)))
  approx <- tee +
    as.numeric(0.5 * !waxing) +
    offset * as.numeric(-1 + 2 * waxing) +
    as.numeric(waxing & (offset <= 0))
  set <- mapply(
    function(approx, loc) {
      binary_search_single(
        approx - hr(6), # lo
        approx + hr(6), # hi
        function(x) observed_lunar_altitude(x, loc) < deg(0), # test_fn
        function(lo, hi) (hi - lo) < mn(1) # end_fn
      )
    },
    approx,
    lst$location,
    SIMPLIFY = TRUE
  )
  out <- pmax(standard_from_universal(set, lst$location), lst$date) # May be just before midnight.
  out[set >= (lst$date + 1)] <- NA
  as_time_of_day(out)
}

#' @rdname sunrise
#' @export
moonrise <- function(date, location) {
  if (!inherits(date, "rdvec")) {
    # Convert to some calendar
    date <- as_gregorian(date)
  }
  date <- vec_data(date)
  lst <- vec_recycle_common(
    date = date,
    location = location,
    .size = max(length(date), length(location))
  )
  tee <- universal_from_standard(lst$date, lst$location) # Midnight.
  waning <- lunar_phase(tee) > deg(180)
  alt <- observed_lunar_altitude(tee, lst$location) # Altitude at midnight.
  lat <- latitude(lst$location)
  offset <- alt / (4 * (deg(90) - abs(lat)))
  approx <- tee +
    as.numeric(0.5 * (!waning)) +
    offset * as.numeric(1 - 2 * waning) +
    as.numeric(waning & (offset > 0))
  rise <- mapply(
    function(approx, loc) {
      binary_search_single(
        approx - hr(6), # lo
        approx + hr(6), # hi
        function(x) observed_lunar_altitude(x, loc) > deg(0), # test_fn
        function(lo, hi) (hi - lo) < mn(1) # end_fn
      )
    },
    approx,
    lst$location,
    SIMPLIFY = TRUE
  )

  out <- pmax(standard_from_universal(rise, lst$location), lst$date) # May be just before midnight.
  out[rise >= (tee + 1)] <- NA
  as_time_of_day(out)
}


#' Full moons and new moons in Gregorian years
#'
#' Calculate all the near-full or near-new moons in a vector of Gregorian years
#'
#' @param year A vector of Gregorian years
#' @return A vector of Gregorian dates representing the full moons or new moons in the given years.
#' @examples
#' full_moons(2025)
#' new_moons(2025)
#' @return A vector of dates
#' @export
new_moons <- function(year) {
  first <- new_moon_at_or_after(vec_data(gregorian_date(min(year), JANUARY, 1)))
  last <- new_moon_before(vec_data(gregorian_date(max(year) + 1, JANUARY, 1)))
  nm <- nth_new_moon(first:last)
  as_gregorian(nm)
}

#' @rdname new_moons
#' @export
full_moons <- function(year) {
  first <- new_moon_at_or_after(gregorian_date(min(year), JANUARY, 1) - 16)
  last <- new_moon_before(gregorian_date(max(year) + 1, JANUARY, 1) + 16)
  nm <- nth_new_moon(first:last)
  fm <- as_gregorian(nm + MEAN_SYNODIC_MONTH / 2)
  y <- granularity(fm, "year")
  fm[y %in% year]
}
