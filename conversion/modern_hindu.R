# Hindu Calendar Functions in R
# Translation from Lisp code

# Helper functions for angles and time
deg <- function(x) x * pi / 180
angle <- function(deg, min, sec) (deg + min/60 + sec/3600) * pi / 180
hr <- function(x) x / 24
mt <- function(x) x

# Modular arithmetic helper
amod <- function(x, n) {
  result <- x %% n
  ifelse(result == 0, n, result)
}

mod3 <- function(x, a, b) {
  # Modular arithmetic with range [a, b)
  ((x - a) %% (b - a)) + a
}

# Location structure
location <- function(latitude, longitude, elevation, time_zone) {
  list(latitude = latitude, longitude = longitude, 
       elevation = elevation, time_zone = time_zone)
}

# Date structures
hindu_lunar_date <- function(year, month, leap_month, day, leap_day) {
  list(year = year, month = month, leap_month = leap_month, 
       day = day, leap_day = leap_day)
}

hindu_solar_date <- function(year, month, day) {
  list(year = year, month = month, day = day)
}

# Accessor functions
hindu_lunar_year <- function(date) date$year
hindu_lunar_month <- function(date) date$month
hindu_lunar_leap_month <- function(date) date$leap_month
hindu_lunar_day <- function(date) date$day
hindu_lunar_leap_day <- function(date) date$leap_day

standard_year <- function(date) date$year
standard_month <- function(date) date$month
standard_day <- function(date) date$day

latitude <- function(loc) loc$latitude
longitude <- function(loc) loc$longitude

# Constants
hindu_sidereal_year <- 365 + 279457/1080000
hindu_epoch <- 0  # Would need actual epoch value
hindu_creation <- hindu_epoch - 1955880000 * hindu_sidereal_year
hindu_sidereal_month <- 27 + 4644439/14438334
hindu_synodic_month <- 29 + 7087771/13358334
hindu_anomalistic_year <- 1577917828000 / (4320000000 - 387)
hindu_anomalistic_month <- 1577917828 / (57753336 - 488199)
hindu_solar_era <- 3179
hindu_lunar_era <- 3044
mean_sidereal_year <- 365.25636  # Approximate value

# Ujjain location
ujjain <- location(angle(23, 9, 0), angle(75, 46, 6), mt(0), hr(5 + 461/9000))
hindu_location <- ujjain

# Hindu sine table simulation
hindu_sine_table <- function(entry) {
  exact <- 3438 * sin(entry * angle(0, 225, 0))
  error <- 0.215 * sign(exact) * sign(abs(exact) - 1716)
  round(exact + error) / 3438
}

# Hindu sine function with linear interpolation
hindu_sine <- function(theta) {
  entry <- theta / angle(0, 225, 0)
  fraction <- entry %% 1
  
  fraction * hindu_sine_table(ceiling(entry)) + 
    (1 - fraction) * hindu_sine_table(floor(entry))
}

# Hindu arcsine function
hindu_arcsin <- function(amp) {
  if (amp < 0) return(-hindu_arcsin(-amp))
  
  # Find position in table
  pos <- 1
  while (pos <= 90 && amp > hindu_sine_table(pos)) {
    pos <- pos + 1
  }
  
  below <- hindu_sine_table(pos - 1)
  angle(0, 225, 0) * (pos - 1 + (amp - below) / 
                      (hindu_sine_table(pos) - below))
}

# Mean position calculation
hindu_mean_position <- function(tee, period) {
  (deg(360) * ((tee - hindu_creation) / period)) %% (2 * pi)
}

# True position calculation
hindu_true_position <- function(tee, period, size, anomalistic, change) {
  lambda <- hindu_mean_position(tee, period)
  offset <- hindu_sine(hindu_mean_position(tee, anomalistic))
  contraction <- abs(offset) * change * size
  equation <- hindu_arcsin(offset * (size - contraction))
  
  (lambda - equation) %% (2 * pi)
}

# Solar longitude
hindu_solar_longitude <- function(tee) {
  hindu_true_position(tee, hindu_sidereal_year, 14/360, 
                      hindu_anomalistic_year, 1/42)
}

# Zodiac sign
hindu_zodiac <- function(tee) {
  1 + floor(hindu_solar_longitude(tee) / deg(30))
}

# Lunar longitude
hindu_lunar_longitude <- function(tee) {
  hindu_true_position(tee, hindu_sidereal_month, 32/360, 
                      hindu_anomalistic_month, 1/96)
}

# Lunar phase
hindu_lunar_phase <- function(tee) {
  (hindu_lunar_longitude(tee) - hindu_solar_longitude(tee)) %% (2 * pi)
}

# Lunar day from moment
hindu_lunar_day_from_moment <- function(tee) {
  1 + floor(hindu_lunar_phase(tee) / deg(12))
}

# Calendar year calculation
hindu_calendar_year <- function(tee) {
  round((tee - hindu_epoch) / hindu_sidereal_year - 
        hindu_solar_longitude(tee) / deg(360))
}

# Equation of time approximation
hindu_equation_of_time <- function(date) {
  offset <- hindu_sine(hindu_mean_position(date, hindu_anomalistic_year))
  equation_sun <- offset * angle(57, 18, 0) * 
                  (14/360 - abs(offset)/1080)
  
  (hindu_daily_motion(date) / deg(360)) * 
    (equation_sun / deg(360)) * hindu_sidereal_year
}

# Daily motion calculation
hindu_daily_motion <- function(date) {
  mean_motion <- deg(360) / hindu_sidereal_year
  anomaly <- hindu_mean_position(date, hindu_anomalistic_year)
  epicycle <- 14/360 - abs(hindu_sine(anomaly))/1080
  
  entry <- floor(anomaly / angle(0, 225, 0))
  sine_table_step <- hindu_sine_table(entry + 1) - hindu_sine_table(entry)
  factor <- -3438/225 * sine_table_step * epicycle
  
  mean_motion * (1 + factor)
}

# Ascensional difference
hindu_ascensional_difference <- function(date, location) {
  sin_delta <- 1397/3438 * hindu_sine(hindu_tropical_longitude(date))
  phi <- latitude(location)
  diurnal_radius <- hindu_sine(deg(90) + hindu_arcsin(sin_delta))
  tan_phi <- hindu_sine(phi) / hindu_sine(deg(90) + phi)
  earth_sine <- sin_delta * tan_phi
  
  hindu_arcsin(earth_sine / diurnal_radius)
}

# Tropical longitude
hindu_tropical_longitude <- function(date) {
  days <- date - hindu_epoch
  precession <- deg(27) - abs(deg(108) * 
                mod3(600/1577917828 * days - 1/4, -1/2, 1/2))
  
  (hindu_solar_longitude(date) - precession) %% (2 * pi)
}

# Rising sign speed
hindu_rising_sign <- function(date) {
  i <- floor(hindu_tropical_longitude(date) / deg(30))
  speeds <- c(1670/1800, 1795/1800, 1935/1800, 1935/1800, 
              1795/1800, 1670/1800)
  speeds[(i %% 6) + 1]
}

# Solar-sidereal difference
hindu_solar_sidereal_difference <- function(date) {
  hindu_daily_motion(date) * hindu_rising_sign(date)
}

# Sunrise calculation
hindu_sunrise <- function(date) {
  date + hr(6) + 
    (longitude(ujjain) - longitude(hindu_location)) / deg(360) -
    hindu_equation_of_time(date) +
    (1577917828/1582237828 / deg(360)) * 
    (hindu_ascensional_difference(date, hindu_location) +
     0.25 * hindu_solar_sidereal_difference(date))
}

# Sunset calculation
hindu_sunset <- function(date) {
  date + hr(18) + 
    (longitude(ujjain) - longitude(hindu_location)) / deg(360) -
    hindu_equation_of_time(date) +
    (1577917828/1582237828 / deg(360)) * 
    (-hindu_ascensional_difference(date, hindu_location) +
     0.75 * hindu_solar_sidereal_difference(date))
}

# Binary search helper
binary_search <- function(lower, upper, condition_func, precision = 1e-10) {
  while (upper - lower > precision) {
    mid <- (lower + upper) / 2
    if (condition_func(mid)) {
      upper <- mid
    } else {
      lower <- mid
    }
  }
  (lower + upper) / 2
}

# New moon before
hindu_new_moon_before <- function(tee) {
  varepsilon <- 2^(-1000)
  tau <- tee - (1/deg(360)) * hindu_lunar_phase(tee) * hindu_synodic_month
  
  binary_search(tau - 1, min(tee, tau + 1), 
                function(x) hindu_lunar_phase(x) < deg(180))
}

# Lunar day at or after
hindu_lunar_day_at_or_after <- function(k, tee) {
  phase <- (k - 1) * deg(12)
  tau <- tee + (1/deg(360)) * 
         ((phase - hindu_lunar_phase(tee)) %% (2 * pi)) * 
         hindu_synodic_month
  
  a <- max(tee, tau - 2)
  b <- tau + 2
  
  # Find moment when lunar phase equals desired phase
  binary_search(a, b, function(x) hindu_lunar_phase(x) >= phase)
}

# Convert from fixed date to Hindu solar
hindu_solar_from_fixed <- function(date) {
  critical <- hindu_sunrise(date + 1)
  month <- hindu_zodiac(critical)
  year <- hindu_calendar_year(critical) - hindu_solar_era
  
  # Find start of month
  approx <- date - 3 - (floor(hindu_solar_longitude(critical)) %% deg(30))
  
  start <- approx
  while (hindu_zodiac(hindu_sunrise(start + 1)) != month) {
    start <- start + 1
  }
  
  day <- date - start + 1
  
  hindu_solar_date(year, month, day)
}

# Convert from Hindu solar to fixed date
fixed_from_hindu_solar <- function(s_date) {
  month <- standard_month(s_date)
  day <- standard_day(s_date)
  year <- standard_year(s_date)
  
  start <- floor((year + hindu_solar_era + (month - 1)/12) * 
                 hindu_sidereal_year) + hindu_epoch
  
  # Search for correct month
  d <- start - 3
  while (hindu_zodiac(hindu_sunrise(d + 1)) != month) {
    d <- d + 1
  }
  
  d + day - 1
}

# Convert from fixed date to Hindu lunar
hindu_lunar_from_fixed <- function(date) {
  critical <- hindu_sunrise(date)
  day <- hindu_lunar_day_from_moment(critical)
  leap_day <- (day == hindu_lunar_day_from_moment(hindu_sunrise(date - 1)))
  
  last_new_moon <- hindu_new_moon_before(critical)
  next_new_moon <- hindu_new_moon_before(floor(last_new_moon) + 35)
  
  solar_month <- hindu_zodiac(last_new_moon)
  leap_month <- (solar_month == hindu_zodiac(next_new_moon))
  
  month <- amod(solar_month + 1, 12)
  
  year_date <- ifelse(month <= 2, date + 180, date)
  year <- hindu_calendar_year(year_date) - hindu_lunar_era
  
  hindu_lunar_date(year, month, leap_month, day, leap_day)
}

# Convert from Hindu lunar to fixed date
fixed_from_hindu_lunar <- function(l_date) {
  year <- hindu_lunar_year(l_date)
  month <- hindu_lunar_month(l_date)
  leap_month <- hindu_lunar_leap_month(l_date)
  day <- hindu_lunar_day(l_date)
  leap_day <- hindu_lunar_leap_day(l_date)
  
  approx <- hindu_epoch + hindu_sidereal_year * 
            (year + hindu_lunar_era + (month - 1)/12)
  
  s <- floor(approx - hindu_sidereal_year * 
             mod3((hindu_solar_longitude(approx) / deg(360)) - 
                  (month - 1)/12, -0.5, 0.5))
  
  k <- hindu_lunar_day_from_moment(s + hr(6))
  
  # Estimate date
  if (k > 3 && k < 27) {
    est <- s - day + k
  } else {
    # Handle borderline cases
    mid <- hindu_lunar_from_fixed(s - 15)
    if (hindu_lunar_month(mid) != month || 
        (hindu_lunar_leap_month(mid) && !leap_month)) {
      est <- s - day + mod3(k, -15, 15)
    } else {
      est <- s - day + mod3(k, 15, 45)
    }
  }
  
  # Refine estimate
  tau <- est - mod3(hindu_lunar_day_from_moment(est + hr(6)) - day, -15, 15)
  
  # Find exact date
  date <- floor(tau)
  while (!(hindu_lunar_day_from_moment(hindu_sunrise(date)) %in% 
           c(day, amod(day + 1, 30)))) {
    date <- date + 1
  }
  
  if (leap_day) date + 1 else date
}

# Lunar station (nakshatra)
hindu_lunar_station <- function(date) {
  critical <- hindu_sunrise(date)
  1 + floor(hindu_lunar_longitude(critical) / angle(0, 800, 0))
}

# Hindu lunar new year
hindu_lunar_new_year <- function(g_year) {
  jan1 <- as.Date(paste0(g_year, "-01-01"))
  # This would need proper implementation with gregorian conversion
  # Simplified version
  jan1_fixed <- as.numeric(jan1)
  
  mina <- hindu_solar_longitude_at_or_after(deg(330), jan1_fixed)
  new_moon <- hindu_lunar_day_at_or_after(1, mina)
  h_day <- floor(new_moon)
  critical <- hindu_sunrise(h_day)
  
  h_day + ifelse(new_moon < critical || 
                 hindu_lunar_day_from_moment(hindu_sunrise(h_day + 1)) == 2, 
                 0, 1)
}

# Solar longitude at or after
hindu_solar_longitude_at_or_after <- function(lambda, tee) {
  tau <- tee + hindu_sidereal_year * (1/deg(360)) * 
         ((lambda - hindu_solar_longitude(tee)) %% (2 * pi))
  
  a <- max(tee, tau - 5)
  b <- tau + 5
  
  binary_search(a, b, function(x) hindu_solar_longitude(x) >= lambda)
}

# Holiday calculations
diwali <- function(g_year) {
  # Diwali is on the new moon day in the month of Kartik (month 8)
  hindu_lunar_holiday(8, 1, g_year)
}

hindu_lunar_holiday <- function(l_month, l_day, g_year) {
  # Get lunar year for the Gregorian year
  jan1 <- as.Date(paste0(g_year, "-01-01"))
  jan1_fixed <- as.numeric(jan1)
  
  l_year <- hindu_lunar_year(hindu_lunar_from_fixed(jan1_fixed))
  
  # Find occurrences in current and next lunar year
  date0 <- hindu_date_occur(l_year, l_month, l_day)
  date1 <- hindu_date_occur(l_year + 1, l_month, l_day)
  
  # Filter dates that fall within the Gregorian year
  dates <- c(date0, date1)
  year_start <- as.numeric(as.Date(paste0(g_year, "-01-01")))
  year_end <- as.numeric(as.Date(paste0(g_year, "-12-31")))
  
  dates[dates >= year_start & dates <= year_end]
}

hindu_date_occur <- function(l_year, l_month, l_day) {
  lunar <- hindu_lunar_date(l_year, l_month, FALSE, l_day, FALSE)
  try_date <- fixed_from_hindu_lunar(lunar)
  
  mid <- hindu_lunar_from_fixed(ifelse(l_day > 15, try_date - 5, try_date))
  expunged <- (l_month != hindu_lunar_month(mid))
  
  if (expunged) {
    l_date <- hindu_lunar_date(hindu_lunar_year(mid), 
                               hindu_lunar_month(mid),
                               hindu_lunar_leap_month(mid), 
                               l_day, FALSE)
    # Find next occurrence
    d <- try_date
    while (hindu_lunar_on_or_before(hindu_lunar_from_fixed(d), l_date)) {
      d <- d + 1
    }
    d - 1
  } else if (l_day != hindu_lunar_day(hindu_lunar_from_fixed(try_date))) {
    try_date - 1
  } else {
    try_date
  }
}

# Comparison function for lunar dates
hindu_lunar_on_or_before <- function(l_date1, l_date2) {
  year1 <- hindu_lunar_year(l_date1)
  year2 <- hindu_lunar_year(l_date2)
  month1 <- hindu_lunar_month(l_date1)
  month2 <- hindu_lunar_month(l_date2)
  day1 <- hindu_lunar_day(l_date1)
  day2 <- hindu_lunar_day(l_date2)
  leap1 <- hindu_lunar_leap_month(l_date1)
  leap2 <- hindu_lunar_leap_month(l_date2)
  leap_day1 <- hindu_lunar_leap_day(l_date1)
  leap_day2 <- hindu_lunar_leap_day(l_date2)
  
  year1 < year2 || 
    (year1 == year2 && 
     (month1 < month2 || 
      (month1 == month2 && 
       (leap1 && !leap2 || 
        (leap1 == leap2 && 
         (day1 < day2 || 
          (day1 == day2 && (!leap_day1 || leap_day2))))))))
}

# Yoga calculation
yoga <- function(date) {
  1 + floor(((hindu_solar_longitude(date) + 
              hindu_lunar_longitude(date)) / angle(0, 800, 0)) %% 27)
}

# Karana calculation
karana <- function(n) {
  if (n == 1) {
    0
  } else if (n > 57) {
    n - 50
  } else {
    amod(n - 1, 7)
  }
}

# Example usage and test functions
cat("Hindu Calendar Functions loaded successfully!\n")
cat("Example usage:\n")
cat("  date <- as.numeric(Sys.Date())\n")
cat("  solar_date <- hindu_solar_from_fixed(date)\n")
cat("  lunar_date <- hindu_lunar_from_fixed(date)\n")
cat("  diwali_dates <- diwali(2024)\n")