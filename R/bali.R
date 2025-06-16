#==============================================================================
# Balinese Calendar
#==============================================================================

BALI_EPOCH <- -1721279 # fixed_from_jd(146)

bali_pawukon_from_fixed <- function(date) {
  out <- list(
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
  # Keep original fixed data so we can invert
  attr(out, "fixed") <- vec_data(date)
  out
}

fixed_from_bali <- function(date) {
  if (!is.null(attributes(date)$fixed)) {
    return(attributes(date)$fixed)
  } else {
    # Without a fixed date, we can't invert
    # So we will assume the last date of the current Gregorian year
    warning("Returning the last occurrence in the current Gregorian year")
    today <- as_gregorian(Sys.Date())
    end_of_year <- gregorian_date(as.list(today)$year, 12, 31)
    return(bali_on_or_before(date, vec_data(end_of_year)))
  }

  #bali_on_or_before(date, BALI_EPOCH)
}

validate_balinese <- function(date) {
  if (any(date$luang < 0 | date$luang > 1)) {
    stop("luang must be either 0 or 1")
  }
  if (any(date$dwiwara < 1 | date$dwiwara > 2)) {
    stop("dwiwara must be either 1 and 2")
  }
  if (any(date$triwara < 1 | date$triwara > 3)) {
    stop("triwara must be between 1 and 3")
  }
  if (any(date$caturwara < 1 | date$caturwara > 4)) {
    stop("caturwara must be between 1 and 4")
  }
  if (any(date$pancawara < 1 | date$pancawara > 5)) {
    stop("pancawara must be between 1 and 5")
  }
  if (any(date$sadwara < 1 | date$sadwara > 6)) {
    stop("sadwara must be between 1 and 6")
  }
  if (any(date$saptawara < 1 | date$saptawara > 7)) {
    stop("saptawara must be between 1 and 7")
  }
  if (any(date$asatawara < 1 | date$asatawara > 8)) {
    stop("asatawara must be between 1 and 8")
  }
  if (any(date$sangawara < 1 | date$sangawara > 9)) {
    stop("sangawara must be between 1 and 9")
  }
  if (any(date$dasawara < 0 | date$dasawara > 9)) {
    stop("dasawara must be between 0 and 9")
  }
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
  validate_balinese,
  format_date,
  bali_pawukon_from_fixed,
  fixed_from_bali
)

#' Balinese Pawukon dates
#'
#' The Balinese calendar repeats every 210 days. It has 10 concurrent
#' weeks, of lengths 1, 2, ..., 10 days. The 210 day cycles are unnumbered,
#' so there is no way to convert a Balinese date into a unique date on
#' another calendar.
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
#' @param dasawara A numeric vector
#' @examples
#' gregorian_date(2025,6,1:10) |>
#'   as_balinese()
#' @seealso [kajeng_keliwon]
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
  dasawara
) {
  new_date(
    luang = luang,
    dwiwara = dwiwara,
    triwara = triwara,
    caturwara = caturwara,
    pancawara = pancawara,
    sadwara = sadwara,
    saptawara = saptawara,
    asatawara = asatawara,
    sangawara = sangawara,
    dasawara = dasawara,
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
  (vec_data(date) - BALI_EPOCH) %% 210
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
  1 + (pmax(6, 4 + (day - 70) %% 210) %% 8)
}

bali_sangawara_from_fixed <- function(date) {
  1 + (pmax(0, (bali_day_from_fixed(date) - 3)) %% 9)
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

# Last fixed date on or before date with Pawukon b_date.
bali_on_or_before <- function(b_date, date) {
  # Position in 5-day subcycle
  a5 <- b_date$pancawara - 1
  # Position in 6-day subcycle
  a6 <- b_date$sadwara - 1
  # Position in 7-day subcycle
  b7 <- b_date$saptawara - 1
  # Position in 35-day subcycle
  b35 <- (a5 + 14 + (15 * (b7 - a5))) %% 35
  # Position in full cycle
  days <- a6 + (36 * (b35 - a6))
  # Reference point
  cap_Delta <- bali_day_from_fixed(0)
  # Return the calculated date
  date - ((date + cap_Delta - days) %% 210)
}

#' Balinese special days
#'
#' Find all occurrences of Kajeng Keliwon and Tumpek in a vector of
#' Gregorian years.
#'
#' @param year A numeric vector of Gregorian years
#' @examples
#' kajeng_keliwon(2025)
#' tumpek(2025)
#'
#' @seealso [balinese_date]
#' @export
kajeng_keliwon <- function(year) {
  yr_range <- vec_data(gregorian_year_range(year))
  cap_Delta <- bali_day_from_fixed(0)
  out <- positions_in_range(8, 15, cap_Delta, yr_range[1], yr_range[2]) |>
    as_gregorian()
  yr <- granularity(out, "year")
  out[yr %in% year]
}

#' @rdname kajeng_keliwon
#' @export
tumpek <- function(year) {
  yr_range <- vec_data(gregorian_year_range(year))
  cap_Delta <- bali_day_from_fixed(0)
  out <- positions_in_range(13, 35, cap_Delta, yr_range[1], yr_range[2]) |>
    as_gregorian()
  yr <- granularity(out, "year")
  out[yr %in% year]
}
