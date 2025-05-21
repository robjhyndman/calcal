#==============================================================================
# Julian Calendar
#==============================================================================

julian_date <- function(year, month, day) {
  c(year, month, day)
}

JULIAN_EPOCH <- fixed_from_gregorian(gregorian_date(0, DECEMBER, 30))

bce <- function(n) {
  -n
}

ce <- function(n) {
  n
}

julian_leap_year <- function(j_year) {
  j_year_mod_4 <- j_year %% 4
  if (j_year > 0) {
    return(j_year_mod_4 == 0)
  } else {
    return(j_year_mod_4 == 3)
  }
}

fixed_from_julian <- function(j_date) {
  month <- standard_month(j_date)
  day <- standard_day(j_date)
  year <- standard_year(j_date)

  y <- ifelse(year < 0, year + 1, year) # No year zero

  JULIAN_EPOCH -
    1 +
    365 * (y - 1) +
    (y - 1) %/% 4 +
    (367 * month - 362) %/% 12 +
    ifelse(month <= 2, 0, ifelse(julian_leap_year(year), -1, -2)) +
    day
}

julian_from_fixed <- function(date) {
  approx <- (4 * (date - JULIAN_EPOCH) + 1464) %/% 1461
  year <- ifelse(approx <= 0, approx - 1, approx) # No year zero

  prior_days <- date - fixed_from_julian(julian_date(year, 1, 1))

  correction <- ifelse(
    date < fixed_from_julian(julian_date(year, 3, 1)),
    0,
    ifelse(julian_leap_year(year), 1, 2)
  )

  month <- (12 * (prior_days + correction) + 373) %/% 367
  day <- date - fixed_from_julian(julian_date(year, month, 1)) + 1

  julian_date(year, month, day)
}

KALENDS <- 1
NONES <- 2
IDES <- 3

roman_date <- function(year, month, event, count, leap) {
  c(year, month, event, count, leap)
}

roman_year <- function(date) {
  date[1]
}

roman_month <- function(date) {
  date[2]
}

roman_event <- function(date) {
  date[3]
}

roman_count <- function(date) {
  date[4]
}

roman_leap <- function(date) {
  date[5]
}

ides_of_month <- function(month) {
  if (month %in% c(MARCH, MAY, JULY, OCTOBER)) {
    15
  } else {
    13
  }
}

nones_of_month <- function(month) {
  ides_of_month(month) - 8
}

fixed_from_roman <- function(r_date) {
  leap <- roman_leap(r_date)
  count <- roman_count(r_date)
  event <- roman_event(r_date)
  month <- roman_month(r_date)
  year <- roman_year(r_date)

  base_date <- switch(
    as.character(event),
    "1" = fixed_from_julian(julian_date(year, month, 1)), # KALENDS
    "2" = fixed_from_julian(julian_date(year, month, nones_of_month(month))), # NONES
    "3" = fixed_from_julian(julian_date(year, month, ides_of_month(month)))
  ) # IDES

  base_date -
    count +
    ifelse(
      julian_leap_year(year) &&
        month == MARCH &&
        event == KALENDS &&
        count >= 6 &&
        count <= 16,
      0,
      1
    ) +
    ifelse(leap, 1, 0)
}

julian_in_gregorian <- function(j_month, j_day, g_year) {
  jan1 <- gregorian_new_year(g_year)
  y <- standard_year(julian_from_fixed(jan1))
  y_prime <- if (y == -1) 1 else y + 1

  date0 <- fixed_from_julian(julian_date(y, j_month, j_day))
  date1 <- fixed_from_julian(julian_date(y_prime, j_month, j_day))

  list_range(c(date0, date1), gregorian_year_range(g_year))
}

YEAR_ROME_FOUNDED <- bce(753)

julian_year_from_auc <- function(year) {
  if (1 <= year && year <= -YEAR_ROME_FOUNDED) {
    year + YEAR_ROME_FOUNDED - 1
  } else {
    year + YEAR_ROME_FOUNDED
  }
}

auc_year_from_julian <- function(year) {
  if (YEAR_ROME_FOUNDED <= year && year <= -1) {
    year - YEAR_ROME_FOUNDED + 1
  } else {
    year - YEAR_ROME_FOUNDED
  }
}

OLYMPIAD_START <- bce(776)

olympiad <- function(cycle, year) {
  c(cycle, year)
}

olympiad_cycle <- function(o_date) {
  o_date[1]
}

olympiad_year <- function(o_date) {
  o_date[2]
}

olympiad_from_julian_year <- function(j_year) {
  years <- j_year - OLYMPIAD_START - ifelse(j_year < 0, 0, 1)
  olympiad(1 + years %/% 4, 1 + years %% 4)
}

julian_year_from_olympiad <- function(o_date) {
  cycle <- olympiad_cycle(o_date)
  year <- olympiad_year(o_date)
  years <- OLYMPIAD_START + 4 * (cycle - 1) + year - 1

  if (years < 0) {
    years
  } else {
    years + 1
  }
}

eastern_orthodox_christmas <- function(g_year) {
  julian_in_gregorian(DECEMBER, 25, g_year)
}
