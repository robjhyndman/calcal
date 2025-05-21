#==============================================================================
# Aztec Calendar
#==============================================================================

aztec_xihuitl_date <- function(month, day) {
  c(month, day)
}

aztec_xihuitl_month <- function(date) {
  date[1]
}

aztec_xihuitl_day <- function(date) {
  date[2]
}

aztec_tonalpohualli_date <- function(number, name) {
  c(number, name)
}

aztec_tonalpohualli_number <- function(date) {
  date[1]
}

aztec_tonalpohualli_name <- function(date) {
  date[2]
}

aztec_xiuhmolpilli_designation <- function(number, name) {
  c(number, name)
}

aztec_xiuhmolpilli_number <- function(date) {
  date[1]
}

aztec_xiuhmolpilli_name <- function(date) {
  date[2]
}

AZTEC_CORRELATION <- fixed_from_julian(julian_date(1521, AUGUST, 13))

aztec_xihuitl_ordinal <- function(x_date) {
  day <- aztec_xihuitl_day(x_date)
  month <- aztec_xihuitl_month(x_date)

  (month - 1) * 20 + (day - 1)
}

AZTEC_XIHUITL_CORRELATION <- AZTEC_CORRELATION -
  aztec_xihuitl_ordinal(aztec_xihuitl_date(11, 2))

aztec_xihuitl_from_fixed <- function(date) {
  count <- (date - AZTEC_XIHUITL_CORRELATION) %% 365
  day <- 1 + count %% 20
  month <- 1 + count %/% 20

  aztec_xihuitl_date(month, day)
}

aztec_tonalpohualli_ordinal <- function(t_date) {
  number <- aztec_tonalpohualli_number(t_date)
  name <- aztec_tonalpohualli_name(t_date)

  (number - 1 + 39 * (number - name)) %% 260
}

AZTEC_TONALPOHUALLI_CORRELATION <- AZTEC_CORRELATION -
  aztec_tonalpohualli_ordinal(aztec_tonalpohualli_date(1, 5))

aztec_tonalpohualli_from_fixed <- function(date) {
  count <- date - AZTEC_TONALPOHUALLI_CORRELATION + 1
  number <- amod(count, 13)
  name <- amod(count, 20)

  aztec_tonalpohualli_date(number, name)
}

aztec_xiuhmolpilli_from_fixed <- function(date) {
  x <- aztec_xihuitl_on_or_before(aztec_xihuitl_date(18, 20), date + 364)
  month <- aztec_xihuitl_month(aztec_xihuitl_from_fixed(date))

  if (month == 19) {
    BOGUS # Nemontemi
  } else {
    aztec_tonalpohualli_from_fixed(x)
  }
}
