#==============================================================================
# Chinese Calendar
#==============================================================================

CHINESE_EPOCH <- -963099 # vec_data(gregorian_date(-2636, FEBRUARY, 15))
CHINESE_MONTH_NAME_EPOCH <- 57
CHINESE_DAY_NAME_EPOCH <- 45

WIDOW <- 0 # Lichun does not occur (double-blind year)
BLIND <- 1 # Lichun occurs once at the end
BRIGHT <- 2 # Lichun occurs once at the start
DOUBLE_BRIGHT <- 3 # Lichun occurs twice (double-happiness)

check_chinese <- function(date) {}

chinese_from_fixed <- function(date) {
  s1 <- chinese_winter_solstice_on_or_before(date) # Prior solstice
  s2 <- chinese_winter_solstice_on_or_before(s1 + 370) # Following solstice
  m12 <- chinese_new_moon_on_or_after(s1 + 1) # Month after last 11th month
  next_m11 <- chinese_new_moon_before(s2 + 1) # Next 11th month
  m <- chinese_new_moon_before(date + 1) # Start of month containing date

  # If there are 13 new moons (12 full lunar months)
  leap_year <- round((next_m11 - m12) / MEAN_SYNODIC_MONTH) == 12

  # Month number
  month <- amod(
    round((m - m12) / MEAN_SYNODIC_MONTH) -
      # Minus 1 during or after a leap month
      (leap_year & chinese_prior_leap_month(m12, m)),
    12
  )

  # It's a leap month if there are 13 months, no major solar term,
  # and no prior leap month
  leap_month <- leap_year &
    chinese_no_major_solar_term(m) &
    !chinese_prior_leap_month(m12, chinese_new_moon_before(m))

  # Approximate since the epoch
  elapsed_years <- floor(
    1.5 + month / 12 + (vec_data(date) - CHINESE_EPOCH) / MEAN_TROPICAL_YEAR
  )

  cycle <- 1 + (elapsed_years - 1) %/% 60
  year <- amod(elapsed_years, 60)
  day <- trunc(1 + vec_data(date) - m)

  list(
    cycle = cycle,
    year = year,
    month = month,
    leap_month = leap_month,
    day = day
  )
}

fixed_from_chinese <- function(c_date) {
  cycle <- c_date$cycle
  year <- c_date$year
  month <- c_date$month
  leap_month <- c_date$leap_month
  day <- c_date$day

  # Middle of the Chinese year
  mid_year <- floor(
    CHINESE_EPOCH +
      ((cycle - 1) * 60 + year - 0.5) *
        MEAN_TROPICAL_YEAR
  )

  new_year <- chinese_new_year_on_or_before(mid_year)

  # New moon before date - a month too early if
  # there was prior leap month that year
  p <- chinese_new_moon_on_or_after(new_year + (month - 1) * 29)
  d <- chinese_from_fixed(p)

  # If the months match, that's the right month
  # Otherwise, there was a prior leap month that year, so we want the next month
  prior_new_moon <- p
  idx <- !(month == d$month & leap_month == d$leap_month)
  if (any(idx)) {
    prior_new_moon[idx] <- chinese_new_moon_on_or_after(p[idx] + 1)
  }
  prior_new_moon + day - 1
}

#' @rdname cal_calendar
#' @format NULL
#' @export
cal_chinese <- cal_calendar(
  name = "chinese",
  short_name = "Chi",
  granularities = c("cycle", "year", "month", "leap_month", "day"),
  check_granularities = check_chinese,
  format = format_date,
  from_rd = chinese_from_fixed,
  to_rd = fixed_from_chinese
)

#' Chinese dates

#' @rdname chinese
#' @param cycle A numeric vector of cycles
#' @param year A numeric vector of years
#' @param month A numeric vector of months
#' @param leap_month A logical vector indicating leap months
#' @param day A numeric vector of days
#' @return A chinese vector object
#' @export
chinese_date <- function(
  cycle = integer(),
  year = integer(),
  month = integer(),
  leap_month = logical(),
  day = integer()
) {
  new_date(
    cycle = cycle,
    year = year,
    month = month,
    leap_month = leap_month,
    day = day,
    calendar = cal_chinese
  )
}

#' @rdname chinese
#' @param date A numeric vector of dates
#' @export
as_chinese <- function(date) {
  as_date(date, calendar = cal_chinese)
}

chinese_location <- function(date) {
  if (inherits(date, "calcalvec")) {
    tee <- vec_data(date)
  } else {
    tee <- date
  }
  year <- gregorian_year_from_fixed(floor(tee))
  out <- location(angle(39, 55, 0), angle(116, 25, 0), mt(43.5), hr(8))
  if (any(year < 1929)) {
    out[year < 1929] <- location(
      angle(39, 55, 0),
      angle(116, 25, 0),
      mt(43.5),
      hr(1397 / 180)
    )
  }
  out
}

chinese_solar_longitude_on_or_after <- function(lambda, tee) {
  sun <- solar_longitude_after(
    lambda,
    universal_from_standard(tee, chinese_location(tee))
  )

  standard_from_universal(sun, chinese_location(sun))
}

current_major_solar_term <- function(date) {
  s <- solar_longitude(
    universal_from_standard(date, chinese_location(date))
  )

  amod(2 + (s %/% deg(30)), 12)
}

major_solar_term_on_or_after <- function(date) {
  s <- solar_longitude(midnight_in_china(date))
  l <- (30 * ceiling(s / 30)) %% 360

  chinese_solar_longitude_on_or_after(l, date)
}

current_minor_solar_term <- function(date) {
  s <- solar_longitude(
    universal_from_standard(date, chinese_location(date))
  )

  amod(3 + ((s - deg(15)) %/% deg(30)), 12)
}

minor_solar_term_on_or_after <- function(date) {
  s <- solar_longitude(midnight_in_china(date))
  l <- (30 * ceiling((s - deg(15)) / 30) + deg(15)) %% 360

  chinese_solar_longitude_on_or_after(l, date)
}

chinese_new_moon_before <- function(date) {
  nm <- new_moon_before(midnight_in_china(date))
  tee <- nth_new_moon(nm)
  floor(standard_from_universal(tee, chinese_location(tee)))
}

chinese_new_moon_on_or_after <- function(date) {
  nm <- new_moon_at_or_after(midnight_in_china(date))
  tee <- nth_new_moon(nm)
  floor(standard_from_universal(tee, chinese_location(tee)))
}


chinese_no_major_solar_term <- function(date) {
  current_major_solar_term(date) ==
    current_major_solar_term(chinese_new_moon_on_or_after(date + 1))
}

midnight_in_china <- function(date) {
  vec_data(universal_from_standard(date, chinese_location(date)))
}

chinese_winter_solstice_on_or_before <- function(date) {
  mapply(
    function(d) {
      approx <- estimate_prior_solar_longitude(WINTER, midnight_in_china(d + 1))

      next_value(floor(approx) - 1, function(day) {
        WINTER < solar_longitude(midnight_in_china(day + 1))
      })
    },
    date,
    SIMPLIFY = TRUE
  )
}


chinese_new_year_in_sui <- function(date) {
  s1 <- chinese_winter_solstice_on_or_before(date) # Prior solstice
  s2 <- chinese_winter_solstice_on_or_before(s1 + 370) # Following solstice
  m12 <- chinese_new_moon_on_or_after(s1 + 1) # Month after 11th month
  m13 <- chinese_new_moon_on_or_after(m12 + 1) # Month after m12
  next_m11 <- chinese_new_moon_before(s2 + 1) # Next 11th month

  # If 13 new moons and either m12 or m13 has no major solar term
  idx <- round((next_m11 - m12) / MEAN_SYNODIC_MONTH) == 12 &
    (chinese_no_major_solar_term(m12) | chinese_no_major_solar_term(m13))
  if (any(idx)) {
    m13[idx] <- chinese_new_moon_on_or_after(m13[idx] + 1)
  }
  m13
}

chinese_new_year_on_or_before <- function(date) {
  new_year <- chinese_new_year_in_sui(date)
  # If date is after the solstice but before the new year,
  # go back half a year
  idx <- date < new_year
  if (any(idx)) {
    new_year <- chinese_new_year_in_sui(date[idx] - 180)
  }
  new_year
}

#' Chinese holidays
#'
#' Dates are returned as Gregorian dates
#'
#' @param year The year on the Gregorian calendar
#' @return A vector of dates on the Gregorian calendar
#' @examples
#' tibble::tibble(
#'   year = 2025:2030,
#'   chinese_new_year(year),
#'   qing_ming(year),
#'   dragon_festival(year)
#' )
#' @export
chinese_new_year <- function(year) {
  chinese_new_year_on_or_before(
    vec_data(gregorian_date(year, JULY, 1))
  ) |>
    as_gregorian()
}

chinese_prior_leap_month <- function(m_prime, m) {
  out <- rep(FALSE, length(m))
  idx <- m >= m_prime
  if (any(idx)) {
    out[idx] <- chinese_no_major_solar_term(m[idx]) |
      chinese_prior_leap_month(m_prime[idx], chinese_new_moon_before(m[idx]))
  }
  out
}

chinese_name <- function(stem, branch) {
  paste(
    c("Jia", "Yi", "Bing", "Ding", "Wu", "Ji", "Geng", "Xun", "Ren", "Gui")[
      stem
    ],
    c(
      "Zi",
      "Chou",
      "Yin",
      "Mao",
      "Chen",
      "Si",
      "Wu",
      "Wei",
      "Shen",
      "You",
      "Xu",
      "Hai"
    )[branch],
    sep = "-"
  )
}

chinese_stem <- function(name) {
  name[1]
}

chinese_branch <- function(name) {
  name[2]
}

chinese_sexagesimal_name <- function(n) {
  chinese_name(amod(n, 10), amod(n, 12))
}

chinese_name_difference <- function(c_name1, c_name2) {
  stem1 <- chinese_stem(c_name1)
  stem2 <- chinese_stem(c_name2)
  branch1 <- chinese_branch(c_name1)
  branch2 <- chinese_branch(c_name2)
  stem_difference <- stem2 - stem1
  branch_difference <- branch2 - branch1

  amod(stem_difference + 25 * (branch_difference - stem_difference), 60)
}

chinese_year_name <- function(year) {
  chinese_sexagesimal_name(year)
}


chinese_month_name <- function(month, year) {
  elapsed_months <- 12 * (year - 1) + (month - 1)

  chinese_sexagesimal_name(elapsed_months - CHINESE_MONTH_NAME_EPOCH)
}


chinese_day_name <- function(date) {
  chinese_sexagesimal_name(date - CHINESE_DAY_NAME_EPOCH)
}

chinese_day_name_on_or_before <- function(name, date) {
  mod3(chinese_name_difference(chinese_day_name(0), name), date, date - 60)
}

#' @rdname chinese_new_year
#' @export
dragon_festival <- function(year) {
  elapsed_years <- 1 + (year - gregorian_year_from_fixed(CHINESE_EPOCH))
  cycle <- 1 + (elapsed_years - 1) %/% 60
  year <- amod(elapsed_years, 60)

  chinese_date(cycle, year, 5, FALSE, 5) |> as_gregorian()
}

#' @rdname chinese_new_year
#' @export
qing_ming <- function(year) {
  floor(minor_solar_term_on_or_after(
    vec_data(gregorian_date(year, MARCH, 30))
  )) |>
    as_gregorian()
}

# birthdate and date are dates on some calendar
# Returns chinese age at date
chinese_age <- function(birthdate, date) {
  date <- vec_data(date)
  birthdate <- vec_data(birthdate)
  today <- chinese_from_fixed(date)
  birthdate_ch <- chinese_from_fixed(birthdate)
  60 * (today$cycle - birthdate_ch$cycle) + (today$year - birthdate_ch$year) + 1
}


chinese_year_marriage_augury <- function(cycle, year) {
  new_year <- fixed_from_chinese(chinese_date(cycle, year, 1, FALSE, 1))
  c <- if (year == 60) cycle + 1 else cycle # Next year's cycle
  y <- if (year == 60) 1 else year + 1 # Next year's number
  next_new_year <- fixed_from_chinese(chinese_date(c, y, 1, FALSE, 1))

  first_minor_term <- current_minor_solar_term(new_year)
  next_first_minor_term <- current_minor_solar_term(next_new_year)

  if (first_minor_term == 1 && next_first_minor_term == 12) {
    WIDOW # No lichun at start or end
  } else if (first_minor_term == 1 && next_first_minor_term != 12) {
    BLIND # No lichun at start, only at end
  } else if (first_minor_term != 1 && next_first_minor_term == 12) {
    BRIGHT # Lichun at start, not at end
  } else {
    DOUBLE_BRIGHT # Lichun at start and end
  }
}
