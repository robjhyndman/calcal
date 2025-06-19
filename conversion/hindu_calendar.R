# Modern Hindu Calendars

# Hindu lunar date structure and accessors
hindu_lunar_date <- function(year, month, leap_month, day, leap_day) {
  list(year = year, month = month, leap_month = leap_month, 
       day = day, leap_day = leap_day)
}

hindu_lunar_month <- function(date) {
  date[[2]]
}

hindu_lunar_leap_month <- function(date) {
  date[[3]]
}

hindu_lunar_day <- function(date) {
  date[[4]]
}

hindu_lunar_leap_day <- function(date) {
  date[[5]]
}

hindu_lunar_year <- function(date) {
  date[[1]]
}

# Mathematical utility functions
hindu_sine_table <- function(entry) {
  # This simulates the Hindu sine table
  # entry is an angle given as a multiplier of 225'
  exact <- 3438 * sin_degrees(entry * angle(0, 225, 0))
  error <- 0.215 * sign(exact) * sign(abs(exact) - 1716)
  round(exact + error) / 3438
}

hindu_sine <- function(theta) {
  # Linear interpolation for theta in Hindu table
  entry <- theta / angle(0, 225, 0)  # Interpolate in table
  fraction <- entry %% 1
  
  fraction * hindu_sine_table(ceiling(entry)) +
  (1 - fraction) * hindu_sine_table(floor(entry))
}

hindu_arcsin <- function(amp) {
  # Inverse of Hindu sine function of amp
  if (amp < 0) {
    -hindu_arcsin(-amp)
  } else {
    pos <- next_func(0, function(k) amp <= hindu_sine_table(k))
    below <- hindu_sine_table(pos - 1)
    
    angle(0, 225, 0) * (pos - 1 + (amp - below) / 
                        (hindu_sine_table(pos) - below))
  }
}

# Astronomical constants
hindu_sidereal_year <- 365 + 279457/1080000

hindu_creation <- hindu_epoch - 1955880000 * hindu_sidereal_year

hindu_sidereal_month <- 27 + 4644439/14438334

hindu_synodic_month <- 29 + 7087771/13358334

hindu_anomalistic_year <- 1577917828000 / (4320000000 - 387)

hindu_anomalistic_month <- 1577917828 / (57753336 - 488199)

hindu_solar_era <- 3179

hindu_lunar_era <- 3044

# Location constants
ujjain <- location(angle(23, 9, 0), angle(75, 46, 6), 
                   mt(0), hr(5 + 461/9000))

hindu_location <- ujjain

# Position calculation functions
hindu_mean_position <- function(tee, period) {
  # Position in degrees at moment tee in uniform circular
  # orbit of period days
  deg(360) * (((tee - hindu_creation) / period) %% 1)
}

hindu_true_position <- function(tee, period, size, anomalistic, change) {
  # Longitudinal position at moment tee
  lambda <- hindu_mean_position(tee, period)
  offset <- hindu_sine(hindu_mean_position(tee, anomalistic))
  contraction <- abs(offset) * change * size
  equation <- hindu_arcsin(offset * (size - contraction))
  
  (lambda - equation) %% 360
}

hindu_solar_longitude <- function(tee) {
  # Solar longitude at moment tee
  hindu_true_position(tee, hindu_sidereal_year, 14/360, 
                      hindu_anomalistic_year, 1/42)
}

hindu_zodiac <- function(tee) {
  #