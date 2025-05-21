#==============================================================================
# Gregorian Calendar - Day and Holiday Functions
#==============================================================================

kday_on_or_before <- function(k, date) {
  date - ((date - k) %% 7)
}

kday_on_or_after <- function(k, date) {
  kday_on_or_before(k, date + 6)
}

kday_nearest <- function(k, date) {
  kday_on_or_before(k, date + 3)
}

kday_after <- function(k, date) {
  kday_on_or_before(k, date + 7)
}

kday_before <- function(k, date) {
  kday_on_or_before(k, date - 1)
}

nth_kday <- function(n, k, g_date) {
  if (n > 0) {
    kday_before(k, fixed_from_gregorian(g_date)) + 7 * n
  } else if (n < 0) {
    kday_after(k, fixed_from_gregorian(g_date)) + 7 * n
  } else {
    BOGUS
  }
}

first_kday <- function(k, g_date) {
  nth_kday(1, k, g_date)
}

last_kday <- function(k, g_date) {
  nth_kday(-1, k, g_date)
}

independence_day <- function(g_year) {
  fixed_from_gregorian(gregorian_date(g_year, JULY, 4))
}

labor_day <- function(g_year) {
  first_kday(MONDAY, gregorian_date(g_year, SEPTEMBER, 1))
}

memorial_day <- function(g_year) {
  last_kday(MONDAY, gregorian_date(g_year, MAY, 31))
}

election_day <- function(g_year) {
  first_kday(TUESDAY, gregorian_date(g_year, NOVEMBER, 2))
}

daylight_saving_start <- function(g_year) {
  nth_kday(2, SUNDAY, gregorian_date(g_year, MARCH, 1))
}

daylight_saving_end <- function(g_year) {
  first_kday(SUNDAY, gregorian_date(g_year, NOVEMBER, 1))
}

christmas <- function(g_year) {
  fixed_from_gregorian(gregorian_date(g_year, DECEMBER, 25))
}

advent <- function(g_year) {
  kday_nearest(
    SUNDAY,
    fixed_from_gregorian(gregorian_date(g_year, NOVEMBER, 30))
  )
}

epiphany <- function(g_year) {
  first_kday(SUNDAY, gregorian_date(g_year, JANUARY, 2))
}

unlucky_fridays <- function(g_year) {
  unlucky_fridays_in_range(gregorian_year_range(g_year))
}

unlucky_fridays_in_range <- function(range) {
  a <- begin(range)
  b <- end(range)
  fri <- kday_on_or_after(FRIDAY, a)
  date <- gregorian_from_fixed(fri)

  if (in_range(fri, range)) {
    result <- if (standard_day(date) == 13) c(fri) else c()
    return(c(result, unlucky_fridays_in_range(interval(fri + 1, b))))
  } else {
    return(c())
  }
}
