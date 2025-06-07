# ==============================================================================
# Hebrew Calendar
# ==============================================================================

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

format_hebrew <- function(x, ...) {
  lst <- attributes(x)$calendar$from_rd(x)
  paste(
    sprintf("%.2d", lst$year),
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
    )[lst$month],
    sprintf("%.2d", lst$day),
    sep = "-"
  )
}

fixed_from_hebrew <- function(date, ...) {
  days_in_prior_months <- mapply(
    function(mth, yr) {
      if (mth < TISHRI) {
        # Before Tishri, add days in months from Tishri to last month of year
        out <- sum(last_day_of_hebrew_month(
          yr,
          TISHRI:last_month_of_hebrew_year(yr)
        ))
        # Add days in months from Nisan to the month before current
        if (mth > 1) {
          out <- out + sum(last_day_of_hebrew_month(yr, NISAN:(mth - 1)))
        }
      } else if (mth > TISHRI) {
        # Just sum days in prior months this year starting from Tishri
        out <- sum(last_day_of_hebrew_month(yr, TISHRI:(mth - 1)))
      } else {
        out <- 0
      }
      out
    },
    date$month,
    date$year,
    SIMPLIFY = TRUE
  )
  hebrew_new_year(date$year) + days_in_prior_months + date$day - 1
}

hebrew_from_fixed <- function(date, ...) {
  # Approximate year (may be off by 1)
  approx <- 1 + (vec_data(date) - HEBREW_EPOCH) %/% (35975351 / 98496)

  # Search forward for year
  ny <- hebrew_new_year(approx)
  ny_next <- hebrew_new_year(approx + 1)
  year <- approx - 1 + (date >= ny) + (date >= ny_next)

  start <- rep(NISAN, length(date))
  start[date < hebrew_date(year, NISAN, 1)] <- TISHRI
  # Find the month
  month <- mapply(
    function(s, y, d) {
      next_value(s, function(m) {
        d <= hebrew_date(y, m, last_day_of_hebrew_month(y, m))
      })
    },
    start,
    year,
    date,
    SIMPLIFY = TRUE
  )
  # Calculate the day by subtraction
  day <- date - hebrew_date(year, month, 1) + 1

  list(year = year, month = month, day = day)
}

#' @rdname cal_calendar
#' @format NULL
#' @export
cal_hebrew <- cal_calendar(
  name = "hebrew",
  short_name = "Heb",
  epoch = 0, # TO REPLACE,
  granularities = c("year", "month", "day"),
  check_granularities = check_hebrew,
  format = format_hebrew,
  from_rd = hebrew_from_fixed,
  to_rd = fixed_from_hebrew
)

#' Work with Hebrew calendar dates
#'
#' @param year A numeric vector of years
#' @param month A numeric vector of months
#' @param day A numeric vector of days
#' @return A hebrew vector object
#' @examples
#' heb <- new_date(year = 5785, month = 3, day = 2:4, calendar = cal_hebrew)
#' heb
#' hebrew_date(5785, 3, 2:4)
#' as_date(heb, calendar = cal_gregorian)
#' as_date(Sys.Date(), calendar = cal_hebrew)
#' tibble::tibble(
#'   gregorian = gregorian_date(2025, 1, 1) + 0:364,
#'   hebrew = as_date(x, calendar = cal_hebrew),
#' )
#' as_gregorian(hebrew_date(5785, 3, 2:4))
#' as_hebrew(gregorian_date(2025, 1, 1:31))
#' as_hebrew("2016-01-01")
#' as_hebrew(Sys.Date())
#' tibble::tibble(
#'   x = seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"),
#'   y = as_hebrew(x)
#' )
#' @rdname hebrew
#' @export
hebrew_date <- function(year = integer(), month = integer(), day = integer()) {
  new_date(year = year, month = month, day = day, calendar = cal_hebrew)
}

#' @param date Vector of dates on some calendar
#' @export
#' @rdname hebrew
as_hebrew <- function(date) {
  as_date(date, calendar = cal_hebrew)
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
  lst <- vec_recycle_common(year = h_year, month = h_month)
  out <- rep(30, length(lst$year))
  out[
    lst$month %in%
      c(IYYAR, TAMMUZ, ELUL, TEVET, ADARII) |
      (lst$month == ADAR & !hebrew_leap_year(lst$year)) |
      (lst$month == MARHESHVAN & !long_marheshvan(lst$year)) |
      (lst$month == KISLEV & short_kislev(lst$year))
  ] <- 29
  out[lst$month > last_month_of_hebrew_year(lst$year)] <- NA
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

fixed_from_molad <- function(moon) {
  r <- (74377 * moon - 2879 / 2160) %% 7
  fixed_from_moment(molad(1, TISHRI) + r * 765433)
}

#' Jewish Holidays
#'
#' Functions to return Gregorian dates for various Jewish holidays
#'
#' @param year A numeric vector of Gregorian years
#' @examples
#' tibble::tibble(
#'   year = 2025:2030,
#'   rosh_hashanah = rosh_hashanah(year),
#'   passover = passover(year),
#'   shavuot = shavuot(year),
#'   yom_kippur = yom_kippur(year),
#'   sukkot = sukkot(year),
#'   hanukkah = hanukkah(year),
#'   purim = purim(year),
#' )
#' @rdname jewish
#' @export
yom_kippur <- function(year) {
  h_year <- 1 + year - gregorian_year_from_fixed(HEBREW_EPOCH)
  as_gregorian(hebrew_date(h_year, TISHRI, 10))
}

#' @rdname jewish
#' @export
passover <- function(year) {
  h_year <- year - gregorian_year_from_fixed(HEBREW_EPOCH)
  as_gregorian(hebrew_date(h_year, NISAN, 15))
}

omer <- function(date) {
  u <- date - passover(gregorian_year_from_fixed(date))
  u[u < 1 | u > 49] <- NA
  c(u %/% 7, u %% 7)
}

#' @rdname jewish
#' @export
purim <- function(year) {
  h_year <- year - gregorian_year_from_fixed(HEBREW_EPOCH)
  last_month <- last_month_of_hebrew_year(h_year)

  as_gregorian(hebrew_date(h_year, last_month, 14))
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
  av9 <- hebrew_date(h_year, AV, 9)

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
    hebrew_date(h_year, last_month_of_hebrew_year(h_year), birth_day)
  } else {
    # Normal anniversary
    hebrew_date(h_year, birth_month, 1) + birth_day - 1
  }
}

hebrew_birthday_in_gregorian <- function(birthdate, g_year) {
  jan1 <- gregorian_new_year(g_year)
  y <- standard_year(as_hebrew(jan1))
  date0 <- hebrew_birthday(birthdate, y)
  date1 <- hebrew_birthday(birthdate, y + 1)
  date2 <- hebrew_birthday(birthdate, y + 2)

  mapply(
    function(d0, d1, d2, year) {
      list_range(c(d0, d1, d2), gregorian_year_range(year))
    },
    date0,
    date1,
    date2,
    g_year
  )
}

yahrzeit <- function(death_date, h_year) {
  death_day <- standard_day(death_date)
  death_month <- standard_month(death_date)
  death_year <- standard_year(death_date)

  out <- hebrew_date(h_year, death_month, 1) + death_day - 1
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
  out[case1] <- hebrew_date(h_year, KISLEV, 1) - 1
  out[case2] <- hebrew_date(h_year, TEVET, 1) - 1
  out[case3] <- hebrew_date(
    h_year,
    last_month_of_hebrew_year(h_year),
    death_day
  )
  out[case4] <- hebrew_date(h_year, SHEVAT, 30)
  out
}

yahrzeit_in_gregorian <- function(death_date, g_year) {
  jan1 <- gregorian_new_year(g_year)
  y <- standard_year(as_hebrew(jan1))
  date0 <- yahrzeit(death_date, y)
  date1 <- yahrzeit(death_date, y + 1)
  date2 <- yahrzeit(death_date, y + 2)
  mapply(
    function(d0, d1, d2, year) {
      list_range(c(d0, d1, d2), gregorian_year_range(year))
    },
    date0,
    date1,
    date2,
    g_year
  )
}

hebrew_in_gregorian <- function(h_month, h_day, g_year) {
  jan1 <- gregorian_new_year(g_year)
  y <- standard_year(as_hebrew(jan1))
  date0 <- hebrew_date(y, h_month, h_day)
  date1 <- hebrew_date(y + 1, h_month, h_day)
  date2 <- hebrew_date(y + 2, h_month, h_day)
  dates3_in_gregorian(g_year, date0, date1, date2)
}

#' @rdname jewish
#' @export
hanukkah <- function(year) {
  as_gregorian(hebrew_in_gregorian(KISLEV, 25, year))
}

#' @rdname jewish
#' @export
rosh_hashanah <- function(year) {
  as_gregorian(hebrew_in_gregorian(TISHRI, 1, year))
}

#' @rdname jewish
#' @export
sukkot <- function(year) {
  as_gregorian(hebrew_in_gregorian(TISHRI, 15, year))
}

#' @rdname jewish
#' @export
shavuot <- function(year) {
  as_gregorian(hebrew_in_gregorian(SIVAN, 6, year))
}
