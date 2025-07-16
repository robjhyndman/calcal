# ==============================================================================
# Chinese Calendar
# ==============================================================================

CHINESE_EPOCH <- -963099 # vec_data(gregorian_date(-2636, FEBRUARY, 15))
CHINESE_MONTH_NAME_EPOCH <- 57
CHINESE_DAY_NAME_EPOCH <- 45

validate_chinese <- function(date) {
  if (any(date$year < 1 | date$year > 60)) {
    stop(
      "year must be between 1 and 60 in the Chinese sexagenary cycle calendar"
    )
  }
  if (any(date$month < 1 | date$month > 12)) {
    stop("month must be between 1 and 12")
  }
  if (any(date$day < 1 | date$day > 30)) {
    stop("day must be between 1 and 30")
  }
}

validate_korean <- function(date) {
  if (any(date$month < 1 | date$month > 12)) {
    stop("month must be between 1 and 12")
  }
  if (any(date$day < 1 | date$day > 30)) {
    stop("day must be between 1 and 30")
  }
}

asian_from_fixed <- function(date, locfn) {
  date <- vec_data(date)
  s1 <- chinese_winter_solstice_on_or_before(date, locfn) # Prior solstice
  s2 <- chinese_winter_solstice_on_or_before(s1 + 370, locfn) # Following solstice
  m12 <- chinese_new_moon_on_or_after(s1 + 1, locfn) # Month after last 11th month
  next_m11 <- chinese_new_moon_before(s2 + 1, locfn) # Next 11th month
  m <- chinese_new_moon_before(date + 1, locfn) # Start of month containing date

  # If there are 13 new moons (12 full lunar months)
  leap_year <- round((next_m11 - m12) / MEAN_SYNODIC_MONTH) == 12

  # Month number
  month <- amod(
    round((m - m12) / MEAN_SYNODIC_MONTH) -
      # Minus 1 during or after a leap month
      (leap_year & chinese_prior_leap_month(m12, m, locfn)),
    12
  )

  # It's a leap month if there are 13 months, no major solar term,
  # and no prior leap month
  leap_month <- leap_year &
    chinese_no_major_solar_term(m, locfn) &
    !chinese_prior_leap_month(m12, chinese_new_moon_before(m, locfn), locfn)

  # Approximate since the epoch
  elapsed_years <- floor(
    1.5 - month / 12 + (date - CHINESE_EPOCH) / MEAN_TROPICAL_YEAR
  )

  cycle <- 1 + (elapsed_years - 1) %/% 60
  year <- amod(elapsed_years, 60)
  day <- trunc(1 + date - m)

  list(
    cycle = cycle,
    year = year,
    month = month,
    leap_month = leap_month,
    day = day
  )
}

fixed_from_asian <- function(date, locfn) {
  # Middle of the Chinese year
  mid_year <- floor(
    CHINESE_EPOCH +
      ((date$cycle - 1) * 60 + date$year - 0.5) *
        MEAN_TROPICAL_YEAR
  )
  new_year <- chinese_new_year_on_or_before(mid_year, locfn)

  # New moon before date - a month too early if
  # there was prior leap month that year
  p <- chinese_new_moon_on_or_after(new_year + (date$month - 1) * 29, locfn)
  d <- chinese_from_fixed(p)

  # If the months match, that's the right month
  # Otherwise, there was a prior leap month that year, so we want the next month
  prior_new_moon <- p
  idx <- !(date$month == d$month & date$leap_month == d$leap_month)
  if (any(idx)) {
    prior_new_moon[idx] <- chinese_new_moon_on_or_after(p[idx] + 1, locfn)
  }
  prior_new_moon + date$day - 1
}

chinese_from_fixed <- function(date) {
  asian_from_fixed(date, chinese_location)
}
fixed_from_chinese <- function(date) {
  fixed_from_asian(date, chinese_location)
}

japanese_from_fixed <- function(date) {
  asian_from_fixed(date, japanese_location)
}
fixed_from_japanese <- function(date) {
  fixed_from_asian(date, japanese_location)
}

korean_from_fixed <- function(date) {
  lst <- asian_from_fixed(date, korean_location)
  lst$year <- korean_year(lst$cycle, lst$year)
  lst$cycle <- NULL
  lst
}

fixed_from_korean <- function(date) {
  date$cycle <- (date$year + 364) / 60
  date$year <- (date$year) %/% (60 * date$cycle)
  fixed_from_asian(date, korean_location)
}

vietnamese_from_fixed <- function(date) {
  asian_from_fixed(date, vietnamese_location)
}
fixed_from_vietnamese <- function(date) {
  fixed_from_asian(date, vietnamese_location)
}

#' @rdname cal_calendar
#' @format NULL
#' @export
cal_chinese <- cal_calendar(
  name = "chinese",
  short_name = "Chi",
  granularities = c("cycle", "year", "month", "leap_month", "day"),
  validate_granularities = validate_chinese,
  format = format_date,
  from_rd = chinese_from_fixed,
  to_rd = fixed_from_chinese
)

#' @rdname cal_calendar
#' @format NULL
#' @export
cal_japanese <- cal_calendar(
  name = "japanese",
  short_name = "Jap",
  granularities = c("cycle", "year", "month", "leap_month", "day"),
  validate_granularities = validate_chinese,
  format = format_date,
  from_rd = japanese_from_fixed,
  to_rd = fixed_from_japanese
)

#' @rdname cal_calendar
#' @format NULL
#' @export
cal_korean <- cal_calendar(
  name = "korean",
  short_name = "Kor",
  granularities = c("year", "month", "leap_month", "day"),
  validate_granularities = validate_korean,
  format = format_date,
  from_rd = korean_from_fixed,
  to_rd = fixed_from_korean
)

#' @rdname cal_calendar
#' @format NULL
#' @export
cal_vietnamese <- cal_calendar(
  name = "vietnamese",
  short_name = "Viet",
  granularities = c("cycle", "year", "month", "leap_month", "day"),
  validate_granularities = validate_chinese,
  format = format_date,
  from_rd = vietnamese_from_fixed,
  to_rd = fixed_from_vietnamese
)

#' Chinese, Japanese, Korean and Vietnamese calendar dates
#'
#' The traditional Chinese lunisolar calendar uses a 60-year cycle with 12 months per year.
#' The Japanese, Korean and Vietnamese calendars are almost identical, but with different
#' locations for determining astronomical positions.
#'
#' @rdname chinese
#' @param cycle A numeric vector of cycles
#' @param year A numeric vector of years within the cycles
#' @param month A numeric vector of months
#' @param leap_month A logical vector indicating leap months
#' @param day A numeric vector of days
#' @return A chinese vector object
#' @seealso [cal_chinese], [chinese_new_year]
#' @examples
#' chinese <- new_date(
#'   cycle = 78, year = 42, month = 5, leap_month = FALSE, day = 16:18,
#'   calendar = cal_chinese
#' )
#' chinese
#' chinese_date(78, 42, 5, FALSE, 16:18)
#' as_date(chinese, calendar = cal_gregorian)
#' as_date(Sys.Date(), calendar = cal_chinese)
#' tibble::tibble(
#'   gregorian = gregorian_date(2025, 1, 1) + 0:364,
#'   chinese = as_chinese(gregorian)
#' )
#' as_gregorian(chinese_date(78, 41, 12, FALSE, 3:30))
#' as_chinese(gregorian_date(2025, 1, 1:28))
#' as_chinese("2016-01-01")
#' as_chinese(Sys.Date())
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
#' @export
japanese_date <- function(
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
    calendar = cal_japanese
  )
}

#' @rdname chinese
#' @export
korean_date <- function(
  year = integer(),
  month = integer(),
  leap_month = logical(),
  day = integer()
) {
  new_date(
    year = year,
    month = month,
    leap_month = leap_month,
    day = day,
    calendar = cal_korean
  )
}

#' @rdname chinese
#' @export
vietnamese_date <- function(
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
    calendar = cal_vietnamese
  )
}

#' @rdname chinese
#' @param date A numeric vector of dates
#' @export
as_chinese <- function(date) {
  as_date(date, calendar = cal_chinese)
}

#' @rdname chinese
#' @param date A numeric vector of dates
#' @export
as_japanese <- function(date) {
  as_date(date, calendar = cal_japanese)
}

#' @rdname chinese
#' @param date A numeric vector of dates
#' @export
as_korean <- function(date) {
  as_date(date, calendar = cal_korean)
}

#' @rdname chinese
#' @param date A numeric vector of dates
#' @export
as_vietnamese <- function(date) {
  as_date(date, calendar = cal_vietnamese)
}

chinese_location <- function(date) {
  tee <- vec_data(date)
  out <- rep(
    location(angle(39, 55, 0), angle(116, 25, 0), 43.5, 8),
    length(tee)
  )
  before_1929 <- tee < 704188 # gregorian_date(1929,1,1) |> vec_data()
  if (any(before_1929)) {
    field(out[before_1929], "zone") <- rep(1397 / 180, length(tee))
  }
  out
}

chinese_solar_longitude_on_or_after <- function(lambda, tee, locfn) {
  sun <- solar_longitude_after(
    lambda,
    universal_from_standard(tee, locfn(tee))
  )

  standard_from_universal(sun, locfn(sun))
}

current_major_solar_term <- function(date, locfn) {
  s <- solar_longitude(
    universal_from_standard(date, locfn(date))
  )

  amod(2 + (s %/% deg(30)), 12)
}

major_solar_term_on_or_after <- function(date, locfn) {
  s <- solar_longitude(midnight_in_china(date, locfn))
  l <- (30 * ceiling(s / 30)) %% 360

  chinese_solar_longitude_on_or_after(l, date, locfn)
}

current_minor_solar_term <- function(date, locfn) {
  s <- solar_longitude(
    universal_from_standard(date, locfn(date))
  )

  amod(3 + ((s - deg(15)) %/% deg(30)), 12)
}

minor_solar_term_on_or_after <- function(date, locfn) {
  s <- solar_longitude(midnight_in_china(date, locfn))
  l <- (30 * ceiling((s - deg(15)) / 30) + deg(15)) %% 360

  chinese_solar_longitude_on_or_after(l, date, locfn)
}

chinese_new_moon_before <- function(date, locfn) {
  nm <- new_moon_before(midnight_in_china(date, locfn))
  tee <- nth_new_moon(nm)
  floor(standard_from_universal(tee, locfn(tee)))
}

chinese_new_moon_on_or_after <- function(date, locfn) {
  nm <- new_moon_at_or_after(midnight_in_china(date, locfn))
  tee <- nth_new_moon(nm)
  floor(standard_from_universal(tee, locfn(tee)))
}

chinese_no_major_solar_term <- function(date, locfn) {
  current_major_solar_term(date, locfn) ==
    current_major_solar_term(
      chinese_new_moon_on_or_after(date + 1, locfn),
      locfn
    )
}

midnight_in_china <- function(date, locfn) {
  vec_data(universal_from_standard(date, locfn(date)))
}

chinese_winter_solstice_on_or_before <- function(date, locfn) {
  approx <- estimate_prior_solar_longitude(
    WINTER,
    midnight_in_china(date + 1, locfn)
  )
  next_value2(floor(approx) - 1, function(day) {
    WINTER < solar_longitude(midnight_in_china(day + 1, locfn))
  })
}

chinese_new_year_in_sui <- function(date, locfn) {
  s1 <- chinese_winter_solstice_on_or_before(date, locfn) # Prior solstice
  s2 <- chinese_winter_solstice_on_or_before(s1 + 370, locfn) # Following solstice
  m12 <- chinese_new_moon_on_or_after(s1 + 1, locfn) # Month after 11th month
  m13 <- chinese_new_moon_on_or_after(m12 + 1, locfn) # Month after m12
  next_m11 <- chinese_new_moon_before(s2 + 1, locfn) # Next 11th month

  # If 13 new moons and either m12 or m13 has no major solar term
  idx <- round((next_m11 - m12) / MEAN_SYNODIC_MONTH) == 12 &
    (chinese_no_major_solar_term(m12, locfn) |
      chinese_no_major_solar_term(m13, locfn))
  if (any(idx)) {
    m13[idx] <- chinese_new_moon_on_or_after(m13[idx] + 1, locfn)
  }
  m13
}

chinese_new_year_on_or_before <- function(date, locfn) {
  new_year <- chinese_new_year_in_sui(date, locfn)
  # If date is after the solstice but before the new year,
  # go back half a year
  idx <- date < new_year
  if (any(idx)) {
    new_year <- chinese_new_year_in_sui(date[idx] - 180, locfn)
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
#'   cny = chinese_new_year(year),
#'   qm = qing_ming(year),
#'   dbf = dragon_festival(year)
#' )
#' @seealso [chinese_date]
#' @export
chinese_new_year <- function(year) {
  chinese_new_year_on_or_before(
    vec_data(gregorian_date(year, JULY, 1)),
    chinese_location
  ) |>
    as_gregorian()
}

chinese_prior_leap_month <- function(m_prime, m, locfn) {
  out <- rep(FALSE, length(m))
  idx <- m >= m_prime
  if (any(idx)) {
    out[idx] <- chinese_no_major_solar_term(m[idx], locfn) |
      chinese_prior_leap_month(
        m_prime[idx],
        chinese_new_moon_before(m[idx], locfn),
        locfn
      )
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
    vec_data(gregorian_date(year, MARCH, 30)),
    chinese_location
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

japanese_location <- function(date) {
  tee <- vec_data(date)
  out <- rep(
    # Longitude 135 time zone
    location(deg(35), deg(135), mt(0), 9),
    length(tee)
  )
  before_1888 <- tee < 689213 # vec_data(gregorian_date(1888,1,1))
  if (any(before_1888)) {
    out[before_1888] <-
      # Tokyo (139 deg 46 min east) local time
      location(deg(35.7), angle(139, 46, 0), mt(24), 9 + 143 / 450)
  }
  out
}

korean_location <- function(date) {
  # Seoul city hall at a varying time zone.
  tee <- vec_data(date)
  z <- rep(9, length(tee))
  case1 <- tee < 696608 #vec_data(gregorian_date(1908, APRIL, 1))
  case2 <- !case1 & tee < 697978 #vec_data(gregorian_date(1912, JANUARY, 1))
  case3 <- !case1 & !case2 & tee < 713398 #vec_data(gregorian_date(1954, MARCH, 21))
  case4 <- !case1 &
    !case2 &
    !case3 &
    tee < 716097 # vec_data(gregorian_date(1961, AUGUST, 10))
  z[case2 | case4] <- 8.5
  location(angle(37, 34, 0), angle(126, 58, 0), mt(0), z)
}

korean_year <- function(cycle, year) {
  # Equivalent Korean year to Chinese cycle and year
  60 * cycle + year - 364
}


vietnamese_location <- function(date) {
  tee <- vec_data(date)
  # Location for Vietnamese calendar is Hanoi; varies with
  # tee. Time zone has changed over the years.
  z <- 7 + (tee < 718432) #vec_data(gregorian_new_year(1968))
  location(angle(21, 2, 0), angle(105, 51, 0), mt(12), z)
}
