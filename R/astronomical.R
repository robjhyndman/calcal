# ==============================================================================
# Section: Time and Astronomy
# ==============================================================================

# Some of original code moved into timezones.R and locale.R

hr <- function(x) {
  # x hours
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
  alpha <- degrees_from_radians(atan(y / x))
  result <- alpha + 180 * (x <= 0)
  result[x == 0 & y != 0] <- sign(y) * 90
  result %% 360
}

arcsin_degrees <- function(x) {
  degrees_from_radians(asin(x))
}

arccos_degrees <- function(x) {
  degrees_from_radians(acos(x))
}

julian_centuries <- function(tee) {
  # Julian centuries since 2000 at moment tee
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

declination <- function(tee, beta, lam) {
  # Return declination at moment UT tee of object at
  # longitude 'lam' and latitude 'beta'
  varepsilon <- obliquity(tee)
  arcsin_degrees(
    (sin_degrees(beta) * cos_degrees(varepsilon)) +
      (cos_degrees(beta) * sin_degrees(varepsilon) * sin_degrees(lam))
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

sine_offset <- function(tee, location, alpha) {
  # Return sine of angle between position of sun at
  # local time tee and when its depression is alpha at location, location.
  # Out of range when it does not occur
  phi <- latitude(location)
  tee_prime <- universal_from_local(tee, location)
  delta <- declination(tee_prime, deg(0), solar_longitude(tee_prime))

  tan_degrees(phi) *
    tan_degrees(delta) +
    sin_degrees(alpha) / (cos_degrees(delta) * cos_degrees(phi))
}

approx_moment_of_depression <- function(tee, loc, alpha, early) {
  try_val <- sine_offset(tee, loc, alpha)
  date <- fixed_from_moment(tee)
  alt <- date
  alt[alpha >= 0] <- date[alpha >= 0] + !early
  alt[alpha < 0] <- date[alpha < 0] + hr(12)
  value <- try_val
  gt1 <- abs(try_val) > 1
  if (any(gt1)) {
    value[gt1] <- sine_offset(alt[gt1], loc, alpha)
  }
  value[value > 1] <- NA_real_
  offset <- mod3(arcsin_degrees(value) / 360, hr(-12), hr(12))
  date <- date +
    as.numeric(early) * (hr(6) - offset) +
    as.numeric(!early) * (hr(18) + offset)
  local_from_apparent(date, loc)
}

moment_of_depression <- function(approx, loc, alpha, early) {
  tee <- approx_moment_of_depression(approx, loc, alpha, early)
  iter <- abs(approx - tee) > sec(30)
  tee[iter] <- approx_moment_of_depression(tee[iter], loc, alpha, early)
  return(tee)
}

dawn <- function(date, locale, alpha) {
  # Standard time in morning of date at locale when depression angle of sun is alpha
  result <- moment_of_depression(vec_data(date) + hr(6), locale, alpha, MORNING)
  standard_from_local(result, locale)
}

dusk <- function(date, locale, alpha) {
  # Standard time in evening on date at locale when depression angle of sun is alpha
  result <- moment_of_depression(
    vec_data(date) + hr(18),
    locale,
    alpha,
    EVENING
  )
  standard_from_local(result, locale)
}

refraction <- function(tee, loc) {
  h <- pmax(mt(0), elevation(loc))
  cap_R <- mt(6.372e6) # Radius of Earth in meters
  dip <- arccos_degrees(cap_R / (cap_R + h)) # Depression of visible horizon

  mins(34) + dip + secs(19) * sqrt(h)
}

#' Sunrise and sunset given a date and location
#'
#' Calculate the time of sunrise and sunset at a specific location and date. The
#' time zone of the location is used as specified in the `location` object.
#'
#' @param date Date in calcalvec format
#' @param location Location of class "location", usually the output from the `location` function
#' @param ... Additional arguments passed to specific methods
#' @return Time of sunrise
#' @examples
#' melbourne <- location(-37.8136, 144.9631, 31, 10)
#' sydney <- location(-33.8688, 151.2093, 3, 10)
#' sunrise(gregorian_date(2025,1,1), c(melbourne, sydney))
#' sunset(gregorian_date(2025,1,1), c(melbourne, sydney))
#' @export
sunrise <- function(date, location, ...) {
  if (inherits(date, "calcalvec")) {
    date <- vec_data(date)
  }
  lst <- vec_recycle_common(
    date = date,
    location = location,
    .size = max(length(date), length(location))
  )
  alpha <- refraction(lst$date + hr(6), lst$location) + mins(16)
  output <- dawn(lst$date, lst$location, alpha)
  as_time_of_day(output)
}

#' @rdname sunrise
#' @export
sunset <- function(date, location, ...) {
  if (inherits(date, "calcalvec")) {
    date <- vec_data(date)
  }
  lst <- vec_recycle_common(
    date = date,
    location = location,
    .size = max(length(date), length(location))
  )
  alpha <- refraction(lst$date + hr(18), lst$location) + mins(16)
  output <- dusk(lst$date, lst$location, alpha)
  as_time_of_day(output)
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
  (sunset_time - sunrise_time) / 12
}

nighttime_temporal_hour <- function(date, loc) {
  next_sunrise <- sunrise(date + 1, loc)
  sunset_time <- sunset(date, loc)
  (next_sunrise - sunset_time) / 12
}
