#==============================================================================
# Hebrew Calendar
#==============================================================================

hebrew_date <- function(year, month, day) {
  c(year, month, day)
}

# Hebrew month constants
NISAN <- 1
IYYAR <- 2
SIVAN <- 3
TAMMUZ <- 4
AV <- 5
ELUL <- 6
TISHRI <- 7
MARHESHVAN <- 8
KISLEV <- 9
TEVET <- 10
SHEVAT <- 11
ADAR <- 12
ADARII <- 13

HEBREW_EPOCH <- fixed_from_julian(julian_date(bce(3761), OCTOBER, 7))

hebrew_leap_year <- function(h_year) {
  (h_year * 7 + 1) %% 19 < 7
}

last_month_of_hebrew_year <- function(h_year) {
  ifelse(hebrew_leap_year(h_year), ADARII, ADAR)
}

hebrew_sabbatical_year <- function(h_year) {
  h_year %% 7 == 0
}

last_day_of_hebrew_month <- function(h_year, h_month) {
  if (
    h_month %in%
      c(IYYAR, TAMMUZ, ELUL, TEVET, ADARII) ||
      (h_month == ADAR && !hebrew_leap_year(h_year)) ||
      (h_month == MARHESHVAN && !long_marheshvan(h_year)) ||
      (h_month == KISLEV && short_kislev(h_year))
  ) {
    return(29)
  } else {
    return(30)
  }
}

molad <- function(h_year, h_month) {
  y <- ifelse(h_month < TISHRI, h_year + 1, h_year)
  months_elapsed <- (y - 1) * 235 %/% 19 + (h_month - TISHRI) %% 12

  HEBREW_EPOCH - 876 / 25920 + months_elapsed * (29 + 12 / 24 + 793 / 25920)
}

hebrew_calendar_elapsed_days <- function(h_year) {
  months_elapsed <- (235 * h_year - 234) %/% 19
  parts_elapsed <- 12084 + 13753 * months_elapsed
  days <- 29 * months_elapsed + parts_elapsed %/% 25920

  if ((3 * (days + 1)) %% 7 < 3) {
    return(days + 1)
  } else {
    return(days)
  }
}

hebrew_new_year <- function(h_year) {
  HEBREW_EPOCH +
    hebrew_calendar_elapsed_days(h_year) +
    hebrew_year_length_correction(h_year)
}

hebrew_year_length_correction <- function(h_year) {
  ny0 <- hebrew_calendar_elapsed_days(h_year - 1)
  ny1 <- hebrew_calendar_elapsed_days(h_year)
  ny2 <- hebrew_calendar_elapsed_days(h_year + 1)

  if (ny2 - ny1 == 356) {
    return(2)
  } else if (ny1 - ny0 == 382) {
    return(1)
  } else {
    return(0)
  }
}

days_in_hebrew_year <- function(h_year) {
  hebrew_new_year(h_year + 1) - hebrew_new_year(h_year)
}

long_marheshvan <- function(h_year) {
  days_in_hebrew_year(h_year) %in% c(355, 385)
}

short_kislev <- function(h_year) {
  days_in_hebrew_year(h_year) %in% c(353, 383)
}

fixed_from_hebrew <- function(h_date) {
  month <- standard_month(h_date)
  day <- standard_day(h_date)
  year <- standard_year(h_date)

  days_in_prior_months <- if (month < TISHRI) {
    # Before Tishri, add days in months from Tishri to last month of year
    sum_prior_months_from_tishri <- sum(sapply(
      TISHRI:last_month_of_hebrew_year(year),
      function(m) last_day_of_hebrew_month(year, m)
    ))

    # Add days in months from Nisan to the month before current
    sum_prior_months_from_nisan <- sum(sapply(
      NISAN:(month - 1),
      function(m) last_day_of_hebrew_month(year, m)
    ))

    sum_prior_months_from_tishri + sum_prior_months_from_nisan
  } else {
    # Just sum days in prior months this year starting from Tishri
    sum(sapply(
      TISHRI:(month - 1),
      function(m) last_day_of_hebrew_month(year, m)
    ))
  }

  hebrew_new_year(year) + days_in_prior_months + day - 1
}

hebrew_from_fixed <- function(date) {
  # Approximate year (may be off by 1)
  approx <- 1 + (date - HEBREW_EPOCH) %/% (35975351 / 98496)

  # Search forward for year
  year <- final_value(approx - 1, function(y) {
    hebrew_new_year(y) <= date
  })

  # Start month search from either Tishri or Nisan
  start <- if (date < fixed_from_hebrew(hebrew_date(year, NISAN, 1))) {
    TISHRI
  } else {
    NISAN
  }

  # Find the month
  month <- next_value(start, function(m) {
    date <=
      fixed_from_hebrew(hebrew_date(
        year,
        m,
        last_day_of_hebrew_month(year, m)
      ))
  })

  # Calculate the day by subtraction
  day <- date - fixed_from_hebrew(hebrew_date(year, month, 1)) + 1

  hebrew_date(year, month, day)
}

fixed_from_molad <- function(moon) {
  r <- (74377 * moon - 2879 / 2160) %% 7
  fixed_from_moment(molad(1, TISHRI) + r * 765433)
}

yom_kippur <- function(g_year) {
  h_year <- 1 + g_year - gregorian_year_from_fixed(HEBREW_EPOCH)
  fixed_from_hebrew(hebrew_date(h_year, TISHRI, 10))
}

passover <- function(g_year) {
  h_year <- g_year - gregorian_year_from_fixed(HEBREW_EPOCH)
  fixed_from_hebrew(hebrew_date(h_year, NISAN, 15))
}

omer <- function(date) {
  c <- date - passover(gregorian_year_from_fixed(date))

  if (1 <= c && c <= 49) {
    return(c(c %/% 7, c %% 7))
  } else {
    return(BOGUS)
  }
}

purim <- function(g_year) {
  h_year <- g_year - gregorian_year_from_fixed(HEBREW_EPOCH)
  last_month <- last_month_of_hebrew_year(h_year)

  fixed_from_hebrew(hebrew_date(h_year, last_month, 14))
}

ta_anit_esther <- function(g_year) {
  purim_date <- purim(g_year)

  if (day_of_week_from_fixed(purim_date) == SUNDAY) {
    purim_date - 3 # Prior Thursday
  } else {
    purim_date - 1 # Previous day
  }
}

tishah_be_av <- function(g_year) {
  h_year <- g_year - gregorian_year_from_fixed(HEBREW_EPOCH)
  av9 <- fixed_from_hebrew(hebrew_date(h_year, AV, 9))

  if (day_of_week_from_fixed(av9) == SATURDAY) {
    av9 + 1 # The next day
  } else {
    av9
  }
}

hebrew_birthday <- function(birthdate, h_year) {
  birth_day <- standard_day(birthdate)
  birth_month <- standard_month(birthdate)
  birth_year <- standard_year(birthdate)

  if (birth_month == last_month_of_hebrew_year(birth_year)) {
    # Same day in last month of year
    fixed_from_hebrew(
      hebrew_date(h_year, last_month_of_hebrew_year(h_year), birth_day)
    )
  } else {
    # Normal anniversary
    fixed_from_hebrew(hebrew_date(h_year, birth_month, 1)) + birth_day - 1
  }
}

hebrew_birthday_in_gregorian <- function(birthdate, g_year) {
  jan1 <- gregorian_new_year(g_year)
  y <- standard_year(hebrew_from_fixed(jan1))

  date0 <- hebrew_birthday(birthdate, y)
  date1 <- hebrew_birthday(birthdate, y + 1)
  date2 <- hebrew_birthday(birthdate, y + 2)

  list_range(c(date0, date1, date2), gregorian_year_range(g_year))
}

yahrzeit <- function(death_date, h_year) {
  death_day <- standard_day(death_date)
  death_month <- standard_month(death_date)
  death_year <- standard_year(death_date)

  if (
    death_month == MARHESHVAN &&
      death_day == 30 &&
      !long_marheshvan(death_year + 1)
  ) {
    # If it's Marheshvan 30, depends on first anniversary
    fixed_from_hebrew(hebrew_date(h_year, KISLEV, 1)) - 1
  } else if (
    death_month == KISLEV && death_day == 30 && short_kislev(death_year + 1)
  ) {
    # If it's Kislev 30, depends on first anniversary
    fixed_from_hebrew(hebrew_date(h_year, TEVET, 1)) - 1
  } else if (death_month == ADARII) {
    # If it's Adar II, use same day in last month of year
    fixed_from_hebrew(
      hebrew_date(h_year, last_month_of_hebrew_year(h_year), death_day)
    )
  } else if (
    death_day == 30 && death_month == ADAR && !hebrew_leap_year(h_year)
  ) {
    # If it's 30th in Adar I and year is not a Hebrew leap year
    fixed_from_hebrew(hebrew_date(h_year, SHEVAT, 30))
  } else {
    # Normal anniversary of date of death
    fixed_from_hebrew(hebrew_date(h_year, death_month, 1)) + death_day - 1
  }
}

yahrzeit_in_gregorian <- function(death_date, g_year) {
  jan1 <- gregorian_new_year(g_year)
  y <- standard_year(hebrew_from_fixed(jan1))

  date0 <- yahrzeit(death_date, y)
  date1 <- yahrzeit(death_date, y + 1)
  date2 <- yahrzeit(death_date, y + 2)

  list_range(c(date0, date1, date2), gregorian_year_range(g_year))
}

hebrew_in_gregorian <- function(h_month, h_day, g_year) {
  jan1 <- gregorian_new_year(g_year)
  y <- standard_year(hebrew_from_fixed(jan1))

  date0 <- fixed_from_hebrew(hebrew_date(y, h_month, h_day))
  date1 <- fixed_from_hebrew(hebrew_date(y + 1, h_month, h_day))
  date2 <- fixed_from_hebrew(hebrew_date(y + 2, h_month, h_day))

  list_range(c(date0, date1, date2), gregorian_year_range(g_year))
}

hanukkah <- function(g_year) {
  hebrew_in_gregorian(KISLEV, 25, g_year)
}
