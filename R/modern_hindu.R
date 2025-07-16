# Hindu Calendar Functions in R
# Ujjain location
UJJAIN <- location(angle(23, 9, 0), angle(75, 46, 6), mt(0), 5 + 461 / 9000)
HINDU_LOCATION <- UJJAIN
# Constants
HINDU_SIDEREAL_YEAR <- 365 + 279457 / 1080000
HINDU_EPOCH <- -1132959 # vec_data(julian_date(bce(3102), FEBRUARY, 18))
HINDU_CREATION <- HINDU_EPOCH - 1955880000 * HINDU_SIDEREAL_YEAR
HINDU_SIDEREAL_MONTH <- 27 + 4644439 / 14438334
HINDU_SYNODIC_MONTH <- 29 + 7087771 / 13358334
HINDU_ANOMALISTIC_YEAR <- 1577917828000 / (4320000000 - 387)
HINDU_ANOMALISTIC_MONTH <- 1577917828 / (57753336 - 488199)
HINDU_SOLAR_ERA <- 3179
HINDU_LUNAR_ERA <- 3044

# Convert from fixed date to Hindu lunar
hindu_lunar_from_fixed <- function(date) {
  date <- vec_data(date)
  critical <- hindu_sunrise(date)
  day <- hindu_lunar_day_from_moment(critical)
  leap_day <- (day == hindu_lunar_day_from_moment(hindu_sunrise(date - 1)))
  last_new_moon <- hindu_new_moon_before(critical)
  next_new_moon <- hindu_new_moon_before(floor(last_new_moon) + 35)
  solar_month <- hindu_zodiac(last_new_moon)
  leap_month <- (solar_month == hindu_zodiac(next_new_moon))
  month <- amod(solar_month + 1, 12)
  year_date <- date + 180 * as.numeric(month <= 2)
  year <- hindu_calendar_year(year_date) - HINDU_LUNAR_ERA
  list(
    year = year,
    month = month,
    leap_month = leap_month,
    day = day,
    leap_day = leap_day
  )
}

# Convert from Hindu lunar to fixed date
fixed_from_hindu_lunar <- function(l_date) {
  approx <- HINDU_EPOCH +
    HINDU_SIDEREAL_YEAR *
      (l_date$year + HINDU_LUNAR_ERA + (l_date$month - 1) / 12)
  s <- floor(
    approx -
      HINDU_SIDEREAL_YEAR *
        mod3(
          (hindu_solar_longitude(approx) / deg(360)) -
            (l_date$month - 1) / 12,
          -0.5,
          0.5
        )
  )
  k <- hindu_lunar_day_from_moment(s + hr(6))
  # Estimate date
  est <- s + l_date$day
  inside <- (3 < k & k < 27)
  mid <- hindu_lunar_from_fixed(s - 15)
  cond2 <- !inside &
    (mid$month != l_date$month |
      (mid$leap_month & !l_date$leap_month))
  cond3 <- !inside & !cond2
  est[inside] <- est[inside] - k[inside]
  est[cond2] <- est[cond2] - mod3(k[cond2], -15, 15)
  est[cond3] <- est[cond3] - mod3(k[cond3], 15, 45)

  # Refine estimate
  tau <- est -
    mod3(hindu_lunar_day_from_moment(est + hr(6)) - l_date$day, -15, 15)

  date <- floor(tau)
  hs <- hindu_lunar_day_from_moment(hindu_sunrise(date))
  hsrange <- c(l_date$day, amod(l_date$day + 1, 30))
  j <- !(hs %in% hsrange)
  while (any(j)) {
    date[j] <- date[j] + 1
    hs[j] <- hindu_lunar_day_from_moment(hindu_sunrise(date[j]))
    j <- !(hs %in% hsrange)
  }

  date + as.numeric(l_date$leap_day)
}

# Convert from fixed date to Hindu solar
hindu_solar_from_fixed <- function(date) {
  date <- vec_data(date)
  critical <- hindu_sunrise(date + 1)
  month <- hindu_zodiac(critical)
  year <- hindu_calendar_year(critical) - HINDU_SOLAR_ERA
  # Find start of month
  approx <- date - 3 - (floor(hindu_solar_longitude(critical)) %% deg(30))
  start <- approx
  j <- hindu_zodiac(hindu_sunrise(start + 1)) != month
  while (any(j)) {
    start[j] <- start[j] + 1
    j <- hindu_zodiac(hindu_sunrise(start + 1)) != month
  }
  day <- date - start + 1
  list(year = year, month = month, day = day)
}

# Convert from Hindu solar to fixed date
fixed_from_hindu_solar <- function(s_date) {
  start <- floor(
    (s_date$year + HINDU_SOLAR_ERA + (s_date$month - 1) / 12) *
      HINDU_SIDEREAL_YEAR
  ) +
    HINDU_EPOCH

  # Search for correct month
  d <- start - 3
  j <- hindu_zodiac(hindu_sunrise(d + 1)) != s_date$month
  while (any(j)) {
    d[j] <- d[j] + 1
    j <- hindu_zodiac(hindu_sunrise(d + 1)) != s_date$month
  }
  d + s_date$day - 1
}


validate_hindu_solar <- function(date) {
  if (any(date$month < 1 | date$month > 12)) {
    stop("month must be between 1 and 12")
  }
  if (any(date$day < 1 | date$day > 32)) {
    stop("day must be between 1 and 32")
  }
}

validate_hindu_lunar <- function(date) {
  if (any(date$month < 1 | date$month > 12)) {
    stop("month must be between 1 and 12")
  }
  if (any(date$day < 1 | date$day > 30)) {
    stop("day must be between 1 and 30")
  }
}

format_hindu_lunar <- function(date) {
  format_date(
    date,
    c(
      "Cait",
      "Vais",
      "Jyes",
      "Asha",
      "Srav",
      "Bhad",
      "Asvi",
      "Kart",
      "Marg",
      "Paus",
      "Magh",
      "Phal"
    )
  )
}

format_hindu_solar <- function(date) {
  format_date(
    date,
    c(
      "Mesa",
      "Vrsh",
      "Mith",
      "Kark",
      "Simh",
      "Kany",
      "Tula",
      "Vrsc",
      "Dhan",
      "Maka",
      "Kumb",
      "Mina"
    )
  )
}

#' @rdname new_calendar
#' @format NULL
#' @export
cal_hindu_lunar <- new_calendar(
  "hindu_lunar",
  "HinL",
  granularities = c("year", "month", "leap_month", "day", "leap_day"),
  validate_hindu_lunar,
  format_hindu_lunar,
  hindu_lunar_from_fixed,
  fixed_from_hindu_lunar
)

#' @rdname new_calendar
#' @format NULL
#' @export
cal_hindu_solar <- new_calendar(
  "hindu_solar",
  "HinS",
  granularities = c("year", "month", "day"),
  validate_hindu_solar,
  format_hindu_solar,
  hindu_solar_from_fixed,
  fixed_from_hindu_solar
)

#' Hindu solar and lunar calendar dates
#'
#' There are four Hindu calendars implemented: modern Hindu solar and lunar calendars,
#' and the old Hindu solar and lunar calendars.
#' Hindu solar months are 1/12 of a solar year (approximately 30.44 days),
#' while lunar months are based on the lunar cycle (approximately 29.53 days).
#'
#' @param year A numeric vector of years
#' @param month A numeric vector of months
#' @param day A numeric vector of days
#' @param leap_month A logical vector indicating if year is a leap year
#' @param leap_day A logical vector indicating if day is a leap day
#' @param date A date vector on some calendar
#' @seealso [cal_hindu_solar], [cal_hindu_lunar], [cal_old_hindu_solar], [cal_old_hindu_lunar], [diwali]
#' @examples
#' gregorian_date(2025, 1, 1:31) |>
#'   as_hindu_solar()
#' gregorian_date(2025, 1, 1:31) |>
#'   as_hindu_lunar()
#' @export
# Date structures
hindu_solar_date <- function(year, month, day) {
  new_date(
    year = year,
    month = month,
    day = day,
    calendar = cal_hindu_solar
  )
}

#' @rdname hindu_solar_date
hindu_lunar_date <- function(year, month, leap_month, day, leap_day) {
  new_date(
    year = year,
    month = month,
    leap_month = leap_month,
    day = day,
    leap_day = leap_day,
    calendar = cal_hindu_lunar
  )
}

#' @rdname hindu_solar_date
#' @export
as_hindu_solar <- function(date) {
  as_date(date, calendar = cal_hindu_solar)
}

#' @rdname hindu_solar_date
#' @export
as_hindu_lunar <- function(date) {
  as_date(date, calendar = cal_hindu_lunar)
}

# Hindu sine table simulation
hindu_sine_table <- function(entry) {
  exact <- 3438 * sin_degrees(entry * angle(0, 225, 0))
  error <- 0.215 * sign(exact) * sign(abs(exact) - 1716)
  round(exact + error) / 3438
}

# Hindu sine function with linear interpolation
hindu_sine <- function(theta) {
  entry <- theta / angle(0, 225, 0)
  fraction <- entry %% 1
  fraction *
    hindu_sine_table(ceiling(entry)) +
    (1 - fraction) * hindu_sine_table(floor(entry))
}

# Hindu arcsine function
hindu_arcsin <- function(amp) {
  out <- numeric(length(amp))
  out[amp < 0] <- -hindu_arcsin_positive(-amp[amp < 0])
  out[amp >= 0] <- hindu_arcsin_positive(amp[amp >= 0])
  out
}
hindu_arcsin_positive <- function(amp) {
  # Find position in table
  pos <- rep(0, length(amp))
  j <- amp > hindu_sine_table(pos)
  while (any(j)) {
    pos[j] <- pos[j] + 1
    j <- amp > hindu_sine_table(pos)
  }

  below <- hindu_sine_table(pos - 1)
  angle(0, 225, 0) *
    (pos -
      1 +
      (amp - below) /
        (hindu_sine_table(pos) - below))
}

# Mean position calculation
hindu_mean_position <- function(tee, period) {
  360 * (((tee - HINDU_CREATION) / period) %% 1)
}

# True position calculation
hindu_true_position <- function(tee, period, size, anomalistic, change) {
  # Longitudinal position at moment tee. period is
  # period of mean motion in days. size is ratio of
  # radii of epicycle and deferent. anomalistic is the
  # period of retrograde revolution about epicycle.
  # change is maximum decrease in epicycle size.
  lambda <- hindu_mean_position(tee, period)
  offset <- hindu_sine(hindu_mean_position(tee, anomalistic))
  contraction <- abs(offset) * change * size
  equation <- hindu_arcsin(offset * (size - contraction))
  (lambda - equation) %% 360
}

# Solar longitude
hindu_solar_longitude <- function(tee) {
  hindu_true_position(
    tee,
    HINDU_SIDEREAL_YEAR,
    14 / 360,
    HINDU_ANOMALISTIC_YEAR,
    1 / 42
  )
}

# Zodiac sign
hindu_zodiac <- function(tee) {
  1 + (hindu_solar_longitude(tee) %/% 30)
}

# Lunar longitude
hindu_lunar_longitude <- function(tee) {
  hindu_true_position(
    tee,
    HINDU_SIDEREAL_MONTH,
    32 / 360,
    HINDU_ANOMALISTIC_MONTH,
    1 / 96
  )
}

# Lunar phase
hindu_lunar_phase <- function(tee) {
  (hindu_lunar_longitude(tee) - hindu_solar_longitude(tee)) %% 360
}

# Lunar day from moment
hindu_lunar_day_from_moment <- function(tee) {
  1 + hindu_lunar_phase(tee) %/% deg(12)
}

# Calendar year calculation
hindu_calendar_year <- function(tee) {
  round(
    (tee - HINDU_EPOCH) /
      HINDU_SIDEREAL_YEAR -
      hindu_solar_longitude(tee) / deg(360)
  )
}

# Equation of time approximation
hindu_equation_of_time <- function(date) {
  offset <- hindu_sine(hindu_mean_position(date, HINDU_ANOMALISTIC_YEAR))
  equation_sun <- offset * angle(57, 18, 0) * (14 / 360 - abs(offset) / 1080)

  (hindu_daily_motion(date) / deg(360)) *
    (equation_sun / deg(360)) *
    HINDU_SIDEREAL_YEAR
}

# Daily motion calculation
hindu_daily_motion <- function(date) {
  mean_motion <- deg(360) / HINDU_SIDEREAL_YEAR
  anomaly <- hindu_mean_position(date, HINDU_ANOMALISTIC_YEAR)
  epicycle <- 14 / 360 - abs(hindu_sine(anomaly)) / 1080

  entry <- floor(anomaly / angle(0, 225, 0))
  sine_table_step <- hindu_sine_table(entry + 1) - hindu_sine_table(entry)
  factor <- -3438 / 225 * sine_table_step * epicycle

  mean_motion * (1 + factor)
}

# Ascensional difference
hindu_ascensional_difference <- function(date, location) {
  sin_delta <- 1397 / 3438 * hindu_sine(hindu_tropical_longitude(date))
  phi <- latitude(location)
  diurnal_radius <- hindu_sine(deg(90) + hindu_arcsin(sin_delta))
  tan_phi <- hindu_sine(phi) / hindu_sine(deg(90) + phi)
  earth_sine <- sin_delta * tan_phi

  hindu_arcsin(-earth_sine / diurnal_radius)
}

# Tropical longitude
hindu_tropical_longitude <- function(date) {
  days <- date - HINDU_EPOCH
  precession <- deg(27) -
    abs(
      deg(108) *
        mod3(600 / 1577917828 * days - 1 / 4, -1 / 2, 1 / 2)
    )

  (hindu_solar_longitude(date) - precession) %% 360
}

# Rising sign speed
hindu_rising_sign <- function(date) {
  i <- floor(hindu_tropical_longitude(date) / deg(30))
  speeds <- c(
    1670 / 1800,
    1795 / 1800,
    1935 / 1800,
    1935 / 1800,
    1795 / 1800,
    1670 / 1800
  )
  speeds[(i %% 6) + 1]
}

# Solar-sidereal difference
hindu_solar_sidereal_difference <- function(date) {
  hindu_daily_motion(date) * hindu_rising_sign(date)
}

# Sunrise calculation
hindu_sunrise <- function(date) {
  date +
    hr(6) +
    (longitude(UJJAIN) - longitude(HINDU_LOCATION)) / deg(360) -
    hindu_equation_of_time(date) +
    (1577917828 / 1582237828 / deg(360)) *
      (hindu_ascensional_difference(date, HINDU_LOCATION) +
        0.25 * hindu_solar_sidereal_difference(date))
}

# Sunset calculation
hindu_sunset <- function(date) {
  date +
    hr(18) +
    (longitude(UJJAIN) - longitude(HINDU_LOCATION)) / deg(360) -
    hindu_equation_of_time(date) +
    (1577917828 / 1582237828 / deg(360)) *
      (-hindu_ascensional_difference(date, HINDU_LOCATION) +
        0.75 * hindu_solar_sidereal_difference(date))
}

# New moon before
hindu_new_moon_before <- function(tee) {
  varepsilon <- 2^(-1000)
  tau <- tee - (1 / deg(360)) * hindu_lunar_phase(tee) * HINDU_SYNODIC_MONTH

  binary_search(
    tau - 1,
    pmin(tee, tau + 1),
    function(x) {
      hindu_lunar_phase(x) < deg(180)
    },
    function(lo, hi) {
      hindu_zodiac(lo) == hindu_zodiac(hi)
    } #{(hi - lo) < varepsilon}
  )
}

# Lunar day at or after
hindu_lunar_day_at_or_after <- function(k, tee) {
  phase <- (k - 1) * deg(12)
  tau <- tee +
    (1 / deg(360)) *
      ((phase - hindu_lunar_phase(tee)) %% 360) *
      HINDU_SYNODIC_MONTH

  a <- pmax(tee, tau - 2)
  b <- tau + 2

  # Find moment when lunar phase equals desired phase
  invert_angular(
    function(x) hindu_lunar_phase(x),
    phase,
    a,
    b
  )
}


# Lunar station (nakshatra)
hindu_lunar_station <- function(date) {
  critical <- hindu_sunrise(date)
  1 + floor(hindu_lunar_longitude(critical) / angle(0, 800, 0))
}

# Hindu lunar new year
#' @rdname diwali
#' @export
hindu_lunar_new_year <- function(year) {
  jan1 <- gregorian_new_year(year) |> vec_data()
  ny <- length(year)

  mina <- hindu_solar_longitude_at_or_after(deg(330), jan1)
  new_moon <- hindu_lunar_day_at_or_after(rep(1, ny), mina)
  h_day <- floor(new_moon)
  critical <- hindu_sunrise(h_day)

  h_day +
    as.numeric(
      !(new_moon < critical |
        hindu_lunar_day_from_moment(hindu_sunrise(h_day + 1)) == 2)
    ) |>
      as_gregorian()
}

# Solar longitude at or after
hindu_solar_longitude_at_or_after <- function(lambda, tee) {
  tau <- tee +
    HINDU_SIDEREAL_YEAR *
      (1 / deg(360)) *
      ((lambda - hindu_solar_longitude(tee)) %% 360)

  a <- pmax(tee, tau - 5)
  b <- tau + 5

  mapply(
    function(a, b) {
      invert_angular(hindu_solar_longitude, lambda, a, b)
    },
    a,
    b,
    SIMPLIFY = TRUE
  )
}

#' @rdname diwali
#' @export
mesha_sankranti <- function(year) {
  # TYPE gregorian-year -> rational-moment
  # Fixed moment of Mesha samkranti (Vernal equinox)
  # in Gregorian year
  jan1 <- gregorian_new_year(year) |> vec_data()
  hindu_solar_longitude_at_or_after(deg(0), jan1) |>
    as_gregorian()
}


#' Hindu holidays and special days
#'
#' Functions to return Gregorian dates for various Hindu holidays based on the
#' Hindu calendars.
#' Hindu Lunar New Year is the first day of the lunar month of Caitra (month 1).
#' The Birthday of Rama is on the 8th or 9th day of the first month (Caitra).
#' Diwali is on the new moon day in the month of Kartik (month 8).
#' The Great Night of Shiva is at the end of the 11 month of Magha.
#' Mesha Sankranti is the day when the sun enters the sign of Aries (Mesha).
#' Sacred Wednesdays are the 8th day of the lunar month that falls on a Wednesday.
#' Dates may vary by a day or two due to variations of the lunar calendar and local traditions.
#'
#' @param year A numeric vector of Gregorian years
#' @return A list of dates on the Gregorian calendar
#' @seealso [hindu_lunar_date]
#' @examples
#' shiva(2025:2026)
#' hindu_lunar_new_year(2025:2026)
#' rama(2025:2026)
#' mesha_sankranti(2025:2026)
#' diwali(2025:2026)
#' sacred_wednesdays(2025:2026)
#' @export
diwali <- function(year) {
  hindu_lunar_holiday(8, 1, year) |>
    unlist() |>
    as_gregorian()
}

hindu_lunar_holiday <- function(l_month, l_day, g_year) {
  # Get lunar year for the Gregorian year
  jan1 <- gregorian_new_year(g_year) |> vec_data()

  l_year <- hindu_lunar_from_fixed(jan1)$year

  # Find occurrences in current and next lunar year
  date0 <- hindu_date_occur(l_year, l_month, l_day)
  date1 <- hindu_date_occur(l_year + 1, l_month, l_day)

  # Filter dates that fall within the Gregorian year
  candidates <- c(date0, date1)
  gyears <- granularity(as_gregorian(candidates), "year")
  sort(unique(candidates[gyears %in% g_year]))
}

hindu_date_occur <- function(l_year, l_month, l_day) {
  # Make all the same length
  date <- vec_recycle_common(year = l_year, month = l_month, day = l_day)
  lunar <- hindu_lunar_date(date$year, date$month, FALSE, date$day, FALSE)
  try_date <- vec_data(lunar)

  mid <- hindu_lunar_from_fixed(try_date - 5 * as.numeric(date$day > 15))
  expunged <- (date$month != mid$month)
  out <- try_date
  if (any(expunged)) {
    l_date <- hindu_lunar_date(
      mid$year[expunged],
      mid$month[expunged],
      mid$leap_month[expunged],
      date$day[expunged],
      FALSE
    )
    # Find next occurrence
    d <- try_date[expunged]
    j <- hindu_lunar_on_or_before(hindu_lunar_from_fixed(d), l_date)
    while (any(j)) {
      d[j] <- d[j] + 1
      j <- hindu_lunar_on_or_before(hindu_lunar_from_fixed(d), l_date)
    }
    out[expunged] <- d - 1
  }
  cond <- rep(FALSE, length(out))
  if (any(!expunged)) {
    cond[!expunged] <- date$day[!expunged] !=
      hindu_lunar_from_fixed(try_date[!expunged])$day
  }
  if (any(cond)) {
    out[cond] <- try_date[cond] - 1
  }
  return(out)
}

hindu_fullmoon_from_fixed <- function(date) {
  # TYPE fixed-date -> hindu-lunar-date
  # Hindu lunar date, full-moon scheme,
  # equivalent to fixed $date$.
  l_date <- hindu_lunar_from_fixed(date)
  year <- l_date$year
  month <- l_date$month
  leap_month <- l_date$leap_month
  day <- l_date$day
  leap_day <- l_date$leap_day
  m <- month
  if (any(day >= 16)) {
    m[day >= 16] <- hindu_lunar_from_fixed(date[day >= 16] + 20)$month
  }
  hindu_lunar_date(year, m, leap_month, day, leap_day)
}

hindu_expunged <- function(l_year, l_month) {
  # TYPE (hindu-lunar-year hindu-lunar-month) ->
  # TYPE  boolean
  # True of Hindu lunar month $l-month$ in $l-year$
  # is expunged.
  l_month !=
    hindu_lunar_from_fixed(
      vec_data(
        hindu_lunar_date(l_year, l_month, FALSE, 15, FALSE)
      )
    )$month
}

fixed_from_hindu_fullmoon <- function(l_date) {
  # TYPE hindu-lunar-date -> fixed-date
  # Fixed date equivalent to Hindu lunar $l-date$
  # in full-moon scheme.
  m <- l_date$month
  cond1 <- (l_date$leap_month | l_date$day <= 15)
  cond2 <- !cond1 & (hindu_expunged(l_date$year, amod(l_date$month - 1, 12)))
  m[cond2] <- amod(l_date$month[cond2] - 2, 12)
  m[!cond1 & !cond2] <- amod(l_date$month[!cond1 & !cond2] - 1, 12)
  vec_data(
    hindu_lunar_date(
      l_date$year,
      m,
      l_date$leap_month,
      l_date$day,
      l_date$leap_day
    )
  )
}


# Comparison function for lunar dates
hindu_lunar_on_or_before <- function(l_date1, l_date2) {
  if (inherits(l_date1, "rdvec")) {
    l_date1 <- attributes(l_date1)$calendar$from_rd(l_date1)
  }
  if (inherits(l_date2, "rdvec")) {
    l_date2 <- attributes(l_date2)$calendar$from_rd(l_date2)
  }
  year1 <- l_date1$year
  year2 <- l_date2$year
  month1 <- l_date1$month
  month2 <- l_date2$month
  day1 <- l_date1$day
  day2 <- l_date2$day
  leap1 <- l_date1$leap_month
  leap2 <- l_date2$leap_month
  leap_day1 <- l_date1$leap_day
  leap_day2 <- l_date2$leap_day

  year1 < year2 |
    (year1 == year2 &
      (month1 < month2 |
        (month1 == month2 &
          (leap1 &
            !leap2 |
            (leap1 == leap2 &
              (day1 < day2 |
                (day1 == day2 & (!leap_day1 | leap_day2))))))))
}

# Yoga calculation
yoga <- function(date) {
  1 +
    floor(
      ((hindu_solar_longitude(date) +
        hindu_lunar_longitude(date)) /
        angle(0, 800, 0)) %%
        27
    )
}

#' @rdname diwali
#' @export
shiva <- function(year) {
  # TYPE gregorian-year -> list-of-fixed-dates
  # List of fixed date(s) of Night of Shiva in Gregorian
  # year $g-year$.
  hindu_lunar_event(11, 29, hr(24), year) |> as_gregorian()
}

#' @rdname diwali
#' @export
rama <- function(year) {
  # TYPE gregorian-year -> list-of-fixed-dates
  # List of fixed date(s) of Rama's Birthday in Gregorian
  # year $g-year$.
  hindu_lunar_event(1, 9, hr(12), year) |> as_gregorian()
}


# Karana calculation
karana <- function(n) {
  out <- amod(n - 1, 7)
  out[n == 1] <- 0
  out[n > 57] <- n - 50
  out
}

#' @rdname diwali
#' @export
sacred_wednesdays <- function(year) {
  # TYPE gregorian-year -> list-of-fixed-dates
  # List of Wednesdays in Gregorian year $g-year$
  # that are day 8 of Hindu lunar months.
  sacred_wednesdays_in_range(
    gregorian_year_range(year) |> vec_data()
  )
}

sacred_wednesdays_in_range <- function(range) {
  # TYPE range -> list-of-fixed-dates
  # List of Wednesdays within $range$ of dates
  # that are day 8 of Hindu lunar months.
  a <- min(range)
  b <- max(range)
  wed <- kday_on_or_after(WEDNESDAY, a)
  wed <- seq(wed, b, by = 7)
  day8 <- granularity(as_hindu_lunar(wed), "day") == 8
  h_date <- hindu_lunar_from_fixed(wed)
  as_gregorian(wed[day8])
}


hindu_standard_from_sundial <- function(tee) {
  # TYPE rational-moment -> rational-moment
  # Hindu local time of temporal moment $tee$.
  date <- fixed_from_moment(tee)
  time <- time_from_moment(tee)
  q <- floor(4 * time) # quarter of day
  a <- if (q == 0) {
    # early this morning
    hindu_sunset(date - 1)
  } else if (q == 3) {
    # this evening
    hindu_sunset(date)
  } else {
    # daytime today
    hindu_sunrise(date)
  }
  b <- if (q == 0) {
    hindu_sunrise(date)
  } else if (q == 3) {
    hindu_sunrise(date + 1)
  } else {
    hindu_sunset(date)
  }
  a +
    2 *
      (b - a) *
      (time -
        if (q == 3) {
          hr(18)
        } else if (q == 0) {
          hr(-6)
        } else {
          hr(6)
        })
}

ayanamsha <- function(tee) {
  # TYPE moment -> angle
  # Difference between tropical and sidereal solar longitude.
  solar_longitude(tee) - sidereal_solar_longitude(tee)
}


hindu_tithi_occur <- function(l_month, tithi, tee, l_year) {
  # TYPE (hindu-lunar-month rational rational
  # TYPE  hindu-lunar-year) -> fixed-date
  # Fixed date of occurrence of Hindu lunar $tithi$ prior
  # to sundial time $tee$, in Hindu lunar $l-month$, $l-year$.
  approx <- hindu_date_occur(l_year, l_month, floor(tithi))
  lunar <- hindu_lunar_day_at_or_after(tithi, approx - 2)
  try <- fixed_from_moment(lunar)
  tee_h <- standard_from_sundial(try + tee, UJJAIN)
  try +
    as.numeric(
      !(lunar <= tee_h |
        hindu_lunar_phase(standard_from_sundial(try + 1 + tee, UJJAIN)) >
          12 * tithi)
    )
}
hindu_lunar_event <- function(l_month, tithi, tee, g_year) {
  # TYPE (hindu-lunar-month rational rational
  # TYPE  gregorian-year) -> list-of-fixed-dates
  # List of fixed dates of occurrences of Hindu lunar $tithi$
  # prior to sundial time $tee$, in Hindu lunar $l-month$,
  # in Gregorian year $g-year$.
  date <- vec_recycle_common(
    month = l_month,
    tithi = tithi,
    tee = tee,
    g_year = g_year
  )

  l_year <-
    hindu_lunar_from_fixed(
      vec_data(gregorian_new_year(date$g_year))
    )
  l_year <- l_year$year
  date0 <- hindu_tithi_occur(date$month, date$tithi, date$tee, l_year)
  date1 <- hindu_tithi_occur(date$month, date$tithi, date$tee, l_year + 1)
  # Filter dates that fall within the Gregorian year
  candidates <- c(date0, date1)
  gyears <- granularity(as_gregorian(candidates), "year")
  sort(unique(candidates[gyears %in% g_year]))
}

standard_from_sundial <- function(tee, location) {
  # TYPE (moment location) -> moment
  # Standard time of temporal moment tee at location.

  date <- fixed_from_moment(tee)
  hour <- 24 * time_from_moment(tee)

  # Determine temporal hour length based on time of day
  daytime <- 6 <= hour & hour <= 18
  early_morning <- hour < 6
  evening <- hour > 18
  h <- rep(NA_real_, length(date))
  if (any(daytime)) {
    h[daytime] <- daytime_temporal_hour(date[daytime], location)
  }
  if (any(early_morning)) {
    h[early_morning] <- nighttime_temporal_hour(
      date[early_morning] - 1,
      location
    )
  }
  if (any(evening)) {
    h[evening] <- nighttime_temporal_hour(date[evening], location)
  }

  # Calculate standard time based on temporal hours
  out <- rep(NA_real_, length(date))
  sunrise_today <- sunrise(date, location)
  sunset_today <- sunset(date, location)
  sunset_yesterday <- sunset(date - 1, location)
  if (any(daytime)) {
    out[daytime] <- sunrise_today + (hour[daytime] - 6) * h[daytime]
  }
  if (any(early_morning)) {
    out[early_morning] <- sunset_yesterday +
      (hour[early_morning] + 6) * h[early_morning]
  }
  if (any(evening)) {
    out[evening] <- sunset_today + (hour[evening] - 18) * h[evening]
  }
  return(out)
}

daytime_temporal_hour <- function(date, location) {
  # Length of daytime temporal hour on fixed date at location.
  (sunset(date, location) - sunrise(date, location)) / 12
}

nighttime_temporal_hour <- function(date, location) {
  # Length of nighttime temporal hour on fixed date at location.
  (sunrise(date + 1, location) - sunset(date, location)) / 12
}

sidereal_solar_longitude <- function(tee) {
  # TYPE moment -> angle
  # Sidereal solar longitude at moment tee

  ((solar_longitude(tee) - precession(tee) + SIDEREAL_START) %% 360)
}


precession <- function(tee) {
  # TYPE moment -> angle
  # Precession at moment tee using 0,0 as J2000 coordinates.
  # Adapted from "Astronomical Algorithms" by Jean Meeus,
  # Willmann-Bell, 2nd edn., 1998, pp. 136-137.

  c <- julian_centuries(tee)

  # Calculate eta (in degrees)
  eta <- poly(c, c(0, secs(47.0029), secs(-0.03302), secs(0.000060))) %% 360

  # Calculate cap-P (in degrees)
  cap_P <- poly(c, c(deg(174.876384), secs(-869.8089), secs(0.03536))) %% 360

  # Calculate p (in degrees)
  p <- poly(c, c(0, secs(5029.0966), secs(1.11113), secs(0.000006))) %% 360

  # Calculate intermediate values
  cap_A <- cos_degrees(eta) * sin_degrees(cap_P)
  cap_B <- cos_degrees(cap_P)

  # Calculate argument
  arg <- arctan_degrees(cap_A, cap_B)

  # Return final precession angle
  (p + cap_P - arg) %% 360
}
