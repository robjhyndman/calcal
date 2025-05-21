#==============================================================================
# Chinese Calendar
#==============================================================================

chinese_date <- function(cycle, year, month, leap, day) {
  c(cycle, year, month, leap, day)
}

chinese_cycle <- function(date) {
  date[1]
}

chinese_year <- function(date) {
  date[2]
}

chinese_month <- function(date) {
  date[3]
}

chinese_leap <- function(date) {
  date[4]
}

chinese_day <- function(date) {
  date[5]
}

chinese_location <- function(tee) {
  year <- gregorian_year_from_fixed(floor(tee))
  if (year < 1929) {
    location(angle(39, 55, 0), angle(116, 25, 0), mt(43.5), hr(1397 / 180))
  } else {
    location(angle(39, 55, 0), angle(116, 25, 0), mt(43.5), hr(8))
  }
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
  tee <- new_moon_before(midnight_in_china(date))

  floor(standard_from_universal(tee, chinese_location(tee)))
}

chinese_new_moon_on_or_after <- function(date) {
  tee <- new_moon_at_or_after(midnight_in_china(date))

  floor(standard_from_universal(tee, chinese_location(tee)))
}

CHINESE_EPOCH <- fixed_from_gregorian(gregorian_date(-2636, FEBRUARY, 15))

chinese_no_major_solar_term <- function(date) {
  current_major_solar_term(date) ==
    current_major_solar_term(chinese_new_moon_on_or_after(date + 1))
}

midnight_in_china <- function(date) {
  universal_from_standard(date, chinese_location(date))
}

chinese_winter_solstice_on_or_before <- function(date) {
  approx <- estimate_prior_solar_longitude(WINTER, midnight_in_china(date + 1))

  next_value(floor(approx) - 1, function(day) {
    WINTER < solar_longitude(midnight_in_china(day + 1))
  })
}

chinese_new_year_in_sui <- function(date) {
  s1 <- chinese_winter_solstice_on_or_before(date) # Prior solstice
  s2 <- chinese_winter_solstice_on_or_before(s1 + 370) # Following solstice
  m12 <- chinese_new_moon_on_or_after(s1 + 1) # Month after 11th month
  m13 <- chinese_new_moon_on_or_after(m12 + 1) # Month after m12
  next_m11 <- chinese_new_moon_before(s2 + 1) # Next 11th month

  # If 13 new moons and either m12 or m13 has no major solar term
  if (
    round((next_m11 - m12) / MEAN_SYNODIC_MONTH) == 12 &&
      (chinese_no_major_solar_term(m12) || chinese_no_major_solar_term(m13))
  ) {
    chinese_new_moon_on_or_after(m13 + 1)
  } else {
    m13
  }
}

chinese_new_year_on_or_before <- function(date) {
  new_year <- chinese_new_year_in_sui(date)

  if (date >= new_year) {
    new_year
  } else {
    # If date is after the solstice but before the new year,
    # go back half a year
    chinese_new_year_in_sui(date - 180)
  }
}

chinese_new_year <- function(g_year) {
  chinese_new_year_on_or_before(
    fixed_from_gregorian(gregorian_date(g_year, JULY, 1))
  )
}

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
      ifelse(leap_year && chinese_prior_leap_month(m12, m), 1, 0),
    12
  )

  # It's a leap month if there are 13 months, no major solar term,
  # and no prior leap month
  leap_month <- leap_year &&
    chinese_no_major_solar_term(m) &&
    !chinese_prior_leap_month(m12, chinese_new_moon_before(m))

  # Approximate since the epoch
  elapsed_years <- floor(
    1.5 + month / 12 + (date - CHINESE_EPOCH) / MEAN_TROPICAL_YEAR
  )

  cycle <- 1 + (elapsed_years - 1) %/% 60
  year <- amod(elapsed_years, 60)
  day <- 1 + date - m

  chinese_date(cycle, year, month, leap_month, day)
}

fixed_from_chinese <- function(c_date) {
  cycle <- chinese_cycle(c_date)
  year <- chinese_year(c_date)
  month <- chinese_month(c_date)
  leap <- chinese_leap(c_date)
  day <- chinese_day(c_date)

  # Middle of the Chinese year
  mid_year <- floor(
    CHINESE_EPOCH + (cycle - 1) * 60 + (year - 1) + 0.5
  ) *
    MEAN_TROPICAL_YEAR

  new_year <- chinese_new_year_on_or_before(mid_year)

  # New moon before date - a month too early if
  # there was prior leap month that year
  p <- chinese_new_moon_on_or_after(new_year + (month - 1) * 29)
  d <- chinese_from_fixed(p)

  # If the months match, that's the right month
  prior_new_moon <- if (month == chinese_month(d) && leap == chinese_leap(d)) {
    p
  } else {
    # Otherwise, there was a prior leap month that year, so we want the next month
    chinese_new_moon_on_or_after(p + 1)
  }

  prior_new_moon + day - 1
}

chinese_prior_leap_month <- function(m_prime, m) {
  if (m >= m_prime) {
    chinese_no_major_solar_term(m) ||
      chinese_prior_leap_month(m_prime, chinese_new_moon_before(m))
  } else {
    FALSE
  }
}

chinese_name <- function(stem, branch) {
  c(stem, branch)
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

CHINESE_MONTH_NAME_EPOCH <- 57

chinese_month_name <- function(month, year) {
  elapsed_months <- 12 * (year - 1) + (month - 1)

  chinese_sexagesimal_name(elapsed_months - CHINESE_MONTH_NAME_EPOCH)
}

CHINESE_DAY_NAME_EPOCH <- rd(45)

chinese_day_name <- function(date) {
  chinese_sexagesimal_name(date - CHINESE_DAY_NAME_EPOCH)
}

chinese_day_name_on_or_before <- function(name, date) {
  mod3(chinese_name_difference(chinese_day_name(0), name), date, date - 60)
}

dragon_festival <- function(g_year) {
  elapsed_years <- 1 + (g_year - gregorian_year_from_fixed(CHINESE_EPOCH))
  cycle <- 1 + (elapsed_years - 1) %/% 60
  year <- amod(elapsed_years, 60)

  fixed_from_chinese(chinese_date(cycle, year, 5, FALSE, 5))
}

qing_ming <- function(g_year) {
  floor(minor_solar_term_on_or_after(
    fixed_from_gregorian(gregorian_date(g_year, MARCH, 30))
  ))
}

chinese_age <- function(birthdate, date) {
  today <- chinese_from_fixed(date)

  if (date >= fixed_from_chinese(birthdate)) {
    60 *
      (chinese_cycle(today) - chinese_cycle(birthdate)) +
      (chinese_year(today) - chinese_year(birthdate)) +
      1
  } else {
    BOGUS
  }
}

WIDOW <- 0 # Lichun does not occur (double-blind year)
BLIND <- 1 # Lichun occurs once at the end
BRIGHT <- 2 # Lichun occurs once at the start
DOUBLE_BRIGHT <- 3 # Lichun occurs twice (double-happiness)

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
