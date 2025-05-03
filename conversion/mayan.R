#==============================================================================
# Mayan Calendars
#==============================================================================

mayan_long_count_date <- function(baktun, katun, tun, uinal, kin) {
  c(baktun, katun, tun, uinal, kin)
}

mayan_haab_date <- function(month, day) {
  c(month, day)
}

mayan_tzolkin_date <- function(number, name) {
  c(number, name)
}

mayan_baktun <- function(date) {
  date[1]
}

mayan_katun <- function(date) {
  date[2]
}

mayan_tun <- function(date) {
  date[3]
}

mayan_uinal <- function(date) {
  date[4]
}

mayan_kin <- function(date) {
  date[5]
}

mayan_haab_month <- function(date) {
  date[1]
}

mayan_haab_day <- function(date) {
  date[2]
}

mayan_tzolkin_number <- function(date) {
  date[1]
}

mayan_tzolkin_name <- function(date) {
  date[2]
}

MAYAN_EPOCH <- fixed_from_jd(584283)  # August 11, -3113

fixed_from_mayan_long_count <- function(count) {
  MAYAN_EPOCH + from_radix(count, c(20, 20, 18, 20))
}

mayan_long_count_from_fixed <- function(date) {
  to_radix(date - MAYAN_EPOCH, c(20, 20, 18, 20))
}

mayan_haab_ordinal <- function(h_date) {
  day <- mayan_haab_day(h_date)
  month <- mayan_haab_month(h_date)
  
  (month - 1) * 20 + day
}

MAYAN_HAAB_EPOCH <- MAYAN_EPOCH - mayan_haab_ordinal(mayan_haab_date(18, 8))

mayan_haab_from_fixed <- function(date) {
  count <- (date - MAYAN_HAAB_EPOCH) %% 365
  day <- count %% 20
  month <- 1 + count %/% 20
  
  mayan_haab_date(month, day)
}

mayan_haab_on_or_before <- function(haab, date) {
  mod3(mayan_haab_ordinal(haab) + MAYAN_HAAB_EPOCH, date, date - 365)
}

mayan_tzolkin_ordinal <- function(t_date) {
  number <- mayan_tzolkin_number(t_date)
  name <- mayan_tzolkin_name(t_date)
  
  (number - 1 + 39 * (number - name)) %% 260
}

MAYAN_TZOLKIN_EPOCH <- MAYAN_EPOCH - mayan_tzolkin_ordinal(mayan_tzolkin_date(4, 20))

mayan_tzolkin_from_fixed <- function(date) {
  count <- date - MAYAN_TZOLKIN_EPOCH + 1
  number <- amod(count, 13)
  name <- amod(count, 20)
  
  mayan_tzolkin_date(number, name)
}

mayan_tzolkin_on_or_before <- function(tzolkin, date) {
  mod3(mayan_tzolkin_ordinal(tzolkin) + MAYAN_TZOLKIN_EPOCH, date, date - 260)
}

mayan_calendar_round_on_or_before <- function(haab, tzolkin, date) {
  haab_count <- mayan_haab_ordinal(haab) + MAYAN_HAAB_EPOCH
  tzolkin_count <- mayan_tzolkin_ordinal(tzolkin) + MAYAN_TZOLKIN_EPOCH
  diff <- tzolkin_count - haab_count
  
  if (diff %% 5 == 0) {
    mod3(haab_count + 365 * diff, date, date - 18980)
  } else {
    BOGUS  # Impossible combination
  }
}
