#==============================================================================
# Balinese Calendar
#==============================================================================

balinese_date <- function(b1, b2, b3, b4, b5, b6, b7, b8, b9, b0) {
  c(b1, b2, b3, b4, b5, b6, b7, b8, b9, b0)
}

bali_luang <- function(b_date) {
  b_date[1]
}

bali_dwiwara <- function(b_date) {
  b_date[2]
}

bali_triwara <- function(b_date) {
  b_date[3]
}

bali_caturwara <- function(b_date) {
  b_date[4]
}

bali_pancawara <- function(b_date) {
  b_date[5]
}

bali_sadwara <- function(b_date) {
  b_date[6]
}

bali_saptawara <- function(b_date) {
  b_date[7]
}

bali_asatawara <- function(b_date) {
  b_date[8]
}

bali_sangawara <- function(b_date) {
  b_date[9]
}

bali_dasawara <- function(b_date) {
  b_date[10]
}

BALI_EPOCH <- fixed_from_jd(146)

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

bali_pawukon_from_fixed <- function(date) {
  balinese_date(
    bali_luang_from_fixed(date),
    bali_dwiwara_from_fixed(date),
    bali_triwara_from_fixed(date),
    bali_caturwara_from_fixed(date),
    bali_pancawara_from_fixed(date),
    bali_sadwara_from_fixed(date),
    bali_saptawara_from_fixed(date),
    bali_asatawara_from_fixed(date),
    bali_sangawara_from_fixed(date),
    bali_dasawara_from_fixed(date)
  )
}

bali_week_from_fixed <- function(date) {
  1 + bali_day_from_fixed(date) %/% 7
}

kajeng_keliwon <- function(g_year) {
  year <- gregorian_year_range(g_year)
  cap_Delta <- bali_day_from_fixed(rd(0))

  positions_in_range(8, 15, cap_Delta, year)
}

tumpek <- function(g_year) {
  year <- gregorian_year_range(g_year)
  cap_Delta <- bali_day_from_fixed(rd(0))

  positions_in_range(13, 35, cap_Delta, year)
}
