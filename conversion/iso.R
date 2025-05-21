#==============================================================================
# ISO Calendar
#==============================================================================

iso_date <- function(year, week, day) {
  c(year, week, day)
}

iso_week <- function(date) {
  date[2]
}

iso_day <- function(date) {
  date[3]
}

iso_year <- function(date) {
  date[1]
}

fixed_from_iso <- function(i_date) {
  week <- iso_week(i_date)
  day <- iso_day(i_date)
  year <- iso_year(i_date)

  nth_kday(week, SUNDAY, gregorian_date(year - 1, DECEMBER, 28)) + day
}

iso_from_fixed <- function(date) {
  approx <- gregorian_year_from_fixed(date - 3)
  year <- if (date >= fixed_from_iso(iso_date(approx + 1, 1, 1))) {
    approx + 1
  } else {
    approx
  }

  week <- 1 + (date - fixed_from_iso(iso_date(year, 1, 1))) %/% 7
  day <- amod(date - rd(0), 7)

  iso_date(year, week, day)
}

iso_long_year <- function(i_year) {
  jan1 <- day_of_week_from_fixed(gregorian_new_year(i_year))
  dec31 <- day_of_week_from_fixed(gregorian_year_end(i_year))

  jan1 == THURSDAY || dec31 == THURSDAY
}
