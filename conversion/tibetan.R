# Tibetan Calendar Functions

tibetan_date <- function(year, month, leap_month, day, leap_day) {
  # Create a tibetan date structure
  list(year = year, month = month, leap_month = leap_month, 
       day = day, leap_day = leap_day)
}

tibetan_month <- function(date) {
  # Extract month from tibetan date
  date[[2]]
}

tibetan_leap_month <- function(date) {
  # Extract leap month from tibetan date
  date[[3]]
}

tibetan_day <- function(date) {
  # Extract day from tibetan date
  date[[4]]
}

tibetan_leap_day <- function(date) {
  # Extract leap day from tibetan date
  date[[5]]
}

tibetan_year <- function(date) {
  # Extract year from tibetan date
  date[[1]]
}

# Tibetan epoch constant
tibetan_epoch <- fixed_from_gregorian(gregorian_date(-127, december, 7))

tibetan_sun_equation <- function(alpha) {
  # Interpolated tabular sine of solar anomaly alpha
  if (alpha > 6) {
    -tibetan_sun_equation(alpha - 6)
  } else if (alpha > 3) {
    tibetan_sun_equation(6 - alpha)
  } else if (alpha == floor(alpha)) {  # integer check
    values <- c(mins(0), mins(6), mins(10), mins(11))
    values[alpha + 1]  # R uses 1-based indexing
  } else {
    (alpha %% 1) * tibetan_sun_equation(ceiling(alpha)) +
    ((-alpha) %% 1) * tibetan_sun_equation(floor(alpha))
  }
}

tibetan_moon_equation <- function(alpha) {
  # Interpolated tabular sine of lunar anomaly alpha
  if (alpha > 14) {
    -tibetan_moon_equation(alpha - 14)
  } else if (alpha > 7) {
    tibetan_moon_equation(14 - alpha)
  } else if (alpha == floor(alpha)) {  # integer check
    values <- c(mins(0), mins(5), mins(10), mins(15), 
                mins(19), mins(22), mins(24), mins(25))
    values[alpha + 1]  # R uses 1-based indexing
  } else {
    (alpha %% 1) * tibetan_moon_equation(ceiling(alpha)) +
    ((-alpha) %% 1) * tibetan_moon_equation(floor(alpha))
  }
}

fixed_from_tibetan <- function(t_date) {
  # Fixed date corresponding to Tibetan lunar date t_date
  year <- tibetan_year(t_date)
  month <- tibetan_month(t_date)
  leap_month <- tibetan_leap_month(t_date)
  day <- tibetan_day(t_date)
  leap_day <- tibetan_leap_day(t_date)
  
  # Lunar month count
  months <- floor(804/65 * (year - 1) + 67/65 * month +
                  ifelse(leap_month, -1, 0) + 64/65)
  
  # Lunar day count
  days <- 30 * months + day
  
  # Mean civil days since epoch
  mean <- days * 11135/11312 - 30 + 
          ifelse(leap_day, 0, -1) + 1071/1616
  
  solar_anomaly <- (days * 13/4824 + 2117/4824) %% 1
  lunar_anomaly <- (days * 3781/105840 + 2837/15120) %% 1
  
  sun <- -tibetan_sun_equation(12 * solar_anomaly)
  moon <- tibetan_moon_equation(28 * lunar_anomaly)
  
  floor(tibetan_epoch + mean + sun + moon)
}

tibetan_from_fixed <- function(date) {
  # Tibetan lunar date corresponding to fixed date
  cap_Y <- 365 + 4975/18382  # Average Tibetan year
  years <- ceiling((date - tibetan_epoch) / cap_Y)
  
  # Search for year
  year0 <- final(years, function(y) {
    date >= fixed_from_tibetan(tibetan_date(y, 1, FALSE, 1, FALSE))
  })
  
  # Search for month
  month0 <- final(1, function(m) {
    date >= fixed_from_tibetan(tibetan_date(year0, m, FALSE, 1, FALSE))
  })
  
  # Estimated day
  est <- date - fixed_from_tibetan(tibetan_date(year0, month0, FALSE, 1, FALSE))
  
  # Search for day
  day0 <- final(est - 2, function(d) {
    date >= fixed_from_tibetan(tibetan_date(year0, month0, FALSE, d, FALSE))
  })
  
  leap_month <- day0 > 30
  day <- amod(day0, 30)
  
  month <- if (day > day0) {
    amod(month0 - 1, 12)
  } else if (leap_month) {
    amod(month0 + 1, 12)
  } else {
    amod(month0, 12)
  }
  
  year <- if (day > day0 && month0 == 1) {
    year0 - 1
  } else if (leap_month && month0 == 12) {
    year0 + 1
  } else {
    year0
  }
  
  leap_day <- (date == fixed_from_tibetan(
    tibetan_date(year, month, leap_month, day, TRUE)))
  
  tibetan_date(year, month, leap_month, day, leap_day)
}

tibetan_leap_month_p <- function(t_year, t_month) {
  # True if t_month is leap in Tibetan year t_year
  t_month == tibetan_month(
    tibetan_from_fixed(
      fixed_from_tibetan(
        tibetan_date(t_year, t_month, TRUE, 2, FALSE))))
}

tibetan_leap_day_p <- function(t_year, t_month, t_day) {
  # True if t_day is leap in Tibetan month t_month and year t_year
  (t_day == tibetan_day(
    tibetan_from_fixed(
      fixed_from_tibetan(
        tibetan_date(t_year, t_month, FALSE, t_day, TRUE))))) ||
  # Check also in leap month if there is one
  (t_day == tibetan_day(
    tibetan_from_fixed(
      fixed_from_tibetan(
        tibetan_date(t_year, t_month,
                     tibetan_leap_month_p(t_year, t_month),
                     t_day, TRUE)))))
}

losar <- function(t_year) {
  # Fixed date of Tibetan New Year (Losar) in Tibetan year t_year
  t_leap <- tibetan_leap_month_p(t_year, 1)
  fixed_from_tibetan(tibetan_date(t_year, 1, t_leap, 1, FALSE))
}

tibetan_new_year <- function(g_year) {
  # List of fixed dates of Tibetan New Year in Gregorian year g_year
  dec31 <- gregorian_year_end(g_year)
  t_year <- tibetan_year(tibetan_from_fixed(dec31))
  
  list_range(
    list(losar(t_year - 1), losar(t_year)),
    gregorian_year_range(g_year))
}
