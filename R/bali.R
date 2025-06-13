#==============================================================================
# Balinese Calendar
#==============================================================================

BALI_EPOCH <- -1721279 # fixed_from_jd(146)

bali_pawukon_from_fixed <- function(date) {
  list(
    luang = bali_luang_from_fixed(date),
    dwiwara = bali_dwiwara_from_fixed(date),
    triwara = bali_triwara_from_fixed(date),
    caturwara = bali_caturwara_from_fixed(date),
    pancawara = bali_pancawara_from_fixed(date),
    sadwara = bali_sadwara_from_fixed(date),
    saptawara = bali_saptawara_from_fixed(date),
    asatawara = bali_asatawara_from_fixed(date),
    sangawara = bali_sangawara_from_fixed(date),
    dasawara = bali_dasawara_from_fixed(date)
  )
}

fixed_from_bali <- function(date) {
  
}

check_balinese <- function(date) {

}

#' @rdname cal_calendar
#' @format NULL
#' @export
cal_balinese <- cal_calendar(
  "bali",
  "Bal",
  c(
    "luang",
    "dwiwara",
    "triwara",
    "caturwara",
    "pancawara",
    "sadwara",
    "saptawara",
    "asatawara",
    "sangawara",
    "dasawara"
  ),
  check_balinese,
  format_date,
  bali_pawukon_from_fixed,
  fixed_from_bali
)

#' Balinese Pawukon dates
#'
#' @param luang A numeric vector
#' @param dwiwara A numeric vector
#' @param triwara A numeric vector
#' @param caturwara A numeric vector
#' @param pancawara A numeric vector
#' @param sadwara A numeric vector
#' @param saptawara A numeric vector
#' @param asatawara A numeric vector
#' @param sangawara A numeric vector
#' @param daswara A numeric vector
#'
#' @export
balinese_date <- function(
  luang,
  dwiwara,
  triwara,
  caturwara,
  pancawara,
  sadwara,
  saptawara,
  asatawara,
  sangawara,
  daswara
) {
  new_date(
    luang,
    dwiwara,
    triwara,
    caturwara,
    pancawara,
    sadwara,
    saptawara,
    asatawara,
    sangawara,
    daswara,
    calendar = cal_balinese
  )
}

#' @rdname balinese_date
#' @param date A vector of dates on some calendar.
#' @export
as_balinese <- function(date) {
  as_date(date, calendar = cal_balinese)
}

bali_day_from_fixed <- function(date) {
  (date - BALI_EPOCH) %% 210
}

bali_luang_from_fixed <- function(date) {
  (bali_dasawara_from_fixed(date) %% 2) == 0
}

bali_dwiwara_from_fixed <- function(date) {
  amod(bali_dasawara_from_fixed(date), 2)
}

bali_triwara_from_fixed <- function(date) {
  1 + (bali_day_from_fixed(date) %% 3)
}

bali_caturwara_from_fixed <- function(date) {
  amod(bali_asatawara_from_fixed(date), 4)
}

bali_pancawara_from_fixed <- function(date) {
  amod(bali_day_from_fixed(date) + 2, 5)
}

bali_sadwara_from_fixed <- function(date) {
  1 + (bali_day_from_fixed(date) %% 6)
}

bali_saptawara_from_fixed <- function(date) {
  1 + (bali_day_from_fixed(date) %% 7)
}

bali_asatawara_from_fixed <- function(date) {
  day <- bali_day_from_fixed(date)
  1 + (max(6, 4 + (day - 70) %% 210) %% 8)
}

bali_sangawara_from_fixed <- function(date) {
  1 + (max(0, (bali_day_from_fixed(date) - 3)) %% 9)
}

bali_dasawara_from_fixed <- function(date) {
  i <- bali_pancawara_from_fixed(date) - 1
  j <- bali_saptawara_from_fixed(date) - 1

  i_values <- c(5, 9, 7, 4, 8)
  j_values <- c(5, 4, 3, 7, 8, 6, 9)

  (1 + i_values[i + 1] + j_values[j + 1]) %% 10
}


bali_week_from_fixed <- function(date) {
  1 + bali_day_from_fixed(date) %/% 7
}

kajeng_keliwon <- function(g_year) {
  year <- gregorian_year_range(g_year)
  cap_Delta <- bali_day_from_fixed(0)

  positions_in_range(8, 15, cap_Delta, year)
}

tumpek <- function(g_year) {
  year <- gregorian_year_range(g_year)
  cap_Delta <- bali_day_from_fixed(0)

  positions_in_range(13, 35, cap_Delta, year)
}
