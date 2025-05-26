# ==============================================================================
# Hebrew Calendar
# ==============================================================================

#' Hebrew calendar dates
#'
#' Create a Hebrew date object.
#'
#' @param year A numeric vector of years
#' @param month A numeric vector of months
#' @param day A numeric vector of days
#' @return A hebrew vector object
#' @examples
#' hebrew_date(2025, 3, 2:4)
#' as_gregorian(hebrew_date(2025, 3, 2:4))
#' as_hebrew(gregorian_date(2025, 1, 1:31))
#' @export
hebrew_date <- function(
  year = integer(),
  month = integer(),
  day = integer()
) {
  lst <- vec_cast_common(year = year, month = month, day = day, .to = integer())
  lst <- vec_recycle_common(
    year = lst$year,
    month = lst$month,
    day = lst$day,
    .size = max(unlist(lapply(lst, length)))
  )
  check_hebrew(lst)
  new_rcrd(lst, class = "hebrew")
}

check_hebrew <- function(args) {
  year <- args$year
  month <- args$month
  day <- args$day
  max_month <- last_month_of_hebrew_year(year)
  if (any(month > max_month)) {
    stop(paste("Too many months in the year", year[month <= max_month]))
  }
  days_month <- last_day_of_hebrew_month(year, month)
  if (any(day > days_month)) {
    stop(paste(
      "Too many days in month",
      year[day > days_month],
      "-",
      month[day > days_month]
    ))
  }
  stopifnot(month >= 1, day >= 1, day <= 30)
}

#' @export
format.hebrew <- function(x, ...) {
  paste(
    sprintf("%.2d", year(x)),
    c(
      "Nisan",
      "Iyar",
      "Sivan",
      "Tammuz",
      "Av",
      "Elul",
      "Tishri",
      "Cheshvan",
      "Kislev",
      "Tevet",
      "Shevat",
      "Adar",
      "Adar II"
    )[field(x, "month")],
    sprintf("%.2d", field(x, "day")),
    sep = "-"
  )
}

as.character.hebrew <- function(x, ...) {
  format(x)
}

hebrew_leap_year <- function(h_year) {
  (h_year * 7 + 1) %% 19 < 7
}

last_month_of_hebrew_year <- function(h_year) {
  out <- rep(ADAR, length(h_year))
  out[hebrew_leap_year(h_year)] <- ADARII
  return(out)
}

hebrew_sabbatical_year <- function(h_year) {
  h_year %% 7 == 0
}

last_day_of_hebrew_month <- function(h_year, h_month) {
  out <- rep(30, length(h_year))
  out[
    h_month %in%
      c(IYYAR, TAMMUZ, ELUL, TEVET, ADARII) |
      (h_month == ADAR & !hebrew_leap_year(h_year)) |
      (h_month == MARHESHVAN & !long_marheshvan(h_year)) |
      (h_month == KISLEV & short_kislev(h_year))
  ] <- 29
  return(out)
}

molad <- function(h_year, h_month) {
  y <- h_year + as.numeric(h_month < TISHRI)
  months_elapsed <- (y - 1) * 235 %/% 19 + (h_month - TISHRI) %% 12
  HEBREW_EPOCH - 876 / 25920 + months_elapsed * (29 + 12 / 24 + 793 / 25920)
}

hebrew_calendar_elapsed_days <- function(h_year) {
  months_elapsed <- (235 * h_year - 234) %/% 19
  parts_elapsed <- 12084 + 13753 * months_elapsed
  days <- 29 * months_elapsed + parts_elapsed %/% 25920
  days <- days + as.numeric((3 * (days + 1)) %% 7 < 3)
  return(days)
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
  correction <- rep(0, length(h_year))
  case1 <- ny2 - ny1 == 356
  case2 <- !case1 & ny1 - ny0 == 382
  correction[case1] <- 2
  correction[case2] <- 1
  return(correction)
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

#' @export
as_rd.hebrew <- function(h_date) {
  month <- standard_month(h_date)
  day <- standard_day(h_date)
  year <- standard_year(h_date)

  days_in_prior_months <- mapply(
    function(mth, yr) {
      if (mth < TISHRI) {
        # Before Tishri, add days in months from Tishri to last month of year
        sum_prior_months_from_tishri <- sum(sapply(
          TISHRI:last_month_of_hebrew_year(yr),
          function(m) last_day_of_hebrew_month(yr, m)
        ))

        # Add days in months from Nisan to the month before current
        if (mth > 1) {
          sum_prior_months_from_nisan <- sum(sapply(
            NISAN:(mth - 1),
            function(m) last_day_of_hebrew_month(yr, m)
          ))
        } else {
          sum_prior_months_from_nisan <- 0
        }
        sum_prior_months_from_tishri + sum_prior_months_from_nisan
      } else {
        # Just sum days in prior months this year starting from Tishri
        sum(sapply(
          TISHRI:(mth - 1),
          function(m) last_day_of_hebrew_month(yr, m)
        ))
      }
    },
    month,
    year,
    SIMPLIFY = TRUE
  )
  hebrew_new_year(year) + days_in_prior_months + day - 1
}

#' Convert to a Hebrew date
#'
#' @param date Vector of dates on some calendar
#' @param ... Additional arguments not currently used
#' @rdname hebrew_date
#' @examples
#' as_hebrew("2016-01-01")
#' as_hebrew(Sys.Date())
#' tibble::tibble(
#'   x = seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"),
#'   y = as_gregorian(x),
#'   z = as_hebrew(x)
#' )
#' @export
as_hebrew <- function(date) {
  UseMethod("as_hebrew", date)
}

#' @export
as_hebrew.default <- function(date, ...) {
  as_hebrew(as_rd(date))
}

#' @export
as_hebrew.rd_fixed <- function(date, ...) {
  # Approximate year (may be off by 1)
  approx <- 1 + (vec_data(date) - HEBREW_EPOCH) %/% (35975351 / 98496)

  # Search forward for year
  year <- mapply(
    function(a, d) {
      final_value(a - 1, function(y) {
        hebrew_new_year(y) <= d
      })
    },
    approx,
    date,
    SIMPLIFY = TRUE
  )

  start <- rep(NISAN, length(date))
  start[date < as_rd(hebrew_date(year, NISAN, 1))] <- TISHRI
  # Find the month
  month <- mapply(
    function(s, y, d) {
      next_value(s, function(m) {
        d <= as_rd(hebrew_date(y, m, last_day_of_hebrew_month(y, m)))
      })
    },
    start,
    year,
    date,
    SIMPLIFY = TRUE
  )

  # Calculate the day by subtraction
  day <- date - as_rd(hebrew_date(year, month, 1)) + 1

  hebrew_date(year, month, day)
}

fixed_from_molad <- function(moon) {
  r <- (74377 * moon - 2879 / 2160) %% 7
  fixed_from_moment(molad(1, TISHRI) + r * 765433)
}

yom_kippur <- function(g_year) {
  h_year <- 1 + g_year - gregorian_year_from_fixed(HEBREW_EPOCH)
  as_rd(hebrew_date(h_year, TISHRI, 10))
}

passover <- function(g_year) {
  h_year <- g_year - gregorian_year_from_fixed(HEBREW_EPOCH)
  as_rd(hebrew_date(h_year, NISAN, 15))
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

  as_rd(hebrew_date(h_year, last_month, 14))
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
  av9 <- as_rd(hebrew_date(h_year, AV, 9))

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
    as_rd(
      hebrew_date(h_year, last_month_of_hebrew_year(h_year), birth_day)
    )
  } else {
    # Normal anniversary
    as_rd(hebrew_date(h_year, birth_month, 1)) + birth_day - 1
  }
}

hebrew_birthday_in_gregorian <- function(birthdate, g_year) {
  jan1 <- gregorian_new_year(g_year)
  y <- standard_year(as_hebrew.rd(jan1))
  date0 <- hebrew_birthday(birthdate, y)
  date1 <- hebrew_birthday(birthdate, y + 1)
  date2 <- hebrew_birthday(birthdate, y + 2)
  list_range(c(date0, date1, date2), gregorian_year_range(g_year))
}

yahrzeit <- function(death_date, h_year) {
  death_day <- standard_day(death_date)
  death_month <- standard_month(death_date)
  death_year <- standard_year(death_date)

  out <- as_rd(hebrew_date(h_year, death_month, 1)) + death_day - 1
  case1 <- death_month == MARHESHVAN &
    death_day == 30 &
    !long_marheshvan(death_year + 1)
  case2 <- !case1 &
    death_month == KISLEV &
    death_day == 30 &
    short_kislev(death_year + 1)
  case3 <- !case1 &
    !case2 &
    death_month == ADARII
  case4 <- !case1 &
    !case2 &
    !case3 &
    death_day == 30 &
    death_month == ADAR &
    !hebrew_leap_year(h_year)
  case5 <- !case1 &
    !case2 &
    !case3 &
    !case4 &
    death_month == ADAR &
    hebrew_leap_year(h_year)
  out[case1] <- as_rd(hebrew_date(h_year, KISLEV, 1)) - 1
  out[case2] <- as_rd(hebrew_date(h_year, TEVET, 1)) - 1
  out[case3] <- as_rd(hebrew_date(
    h_year,
    last_month_of_hebrew_year(h_year),
    death_day
  ))
  out[case4] <- as_rd(hebrew_date(h_year, SHEVAT, 30))
  out
}

yahrzeit_in_gregorian <- function(death_date, g_year) {
  jan1 <- gregorian_new_year(g_year)
  y <- standard_year(as_hebrew.rd(jan1))
  date0 <- yahrzeit(death_date, y)
  date1 <- yahrzeit(death_date, y + 1)
  date2 <- yahrzeit(death_date, y + 2)
  list_range(c(date0, date1, date2), gregorian_year_range(g_year))
}

hebrew_in_gregorian <- function(h_month, h_day, g_year) {
  jan1 <- gregorian_new_year(g_year)
  y <- standard_year(as_hebrew(jan1))
  date0 <- as_rd(hebrew_date(y, h_month, h_day))
  date1 <- as_rd(hebrew_date(y + 1, h_month, h_day))
  date2 <- as_rd(hebrew_date(y + 2, h_month, h_day))
  list_range(c(date0, date1, date2), gregorian_year_range(g_year))
}

hanukkah <- function(g_year) {
  hebrew_in_gregorian(KISLEV, 25, g_year)
}
