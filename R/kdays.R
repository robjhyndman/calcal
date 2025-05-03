
kday_on_or_before <- function(k, date) {
  # Fixed date of the k-day on or before fixed date.
  # k=0 means Sunday, k=1 means Monday, and so on.
  date - ((vec_data(date) - k) %% 7L)
}

kday_on_or_after <- function(k, date) {
  # Fixed date of the k-day on or after fixed date.
  # k=0 means Sunday, k=1 means Monday, and so on.
  kday_on_or_before(k, date + 6)
}

kday_nearest <- function(k, date) {
  # Fixed date of the k-day nearest fixed date.
  # k=0 means Sunday, k=1 means Monday, and so on.
  kday_on_or_before(k, date + 3)
}

kday_after <- function(k, date) {
  # Fixed date of the k-day after fixed date.
  # k=0 means Sunday, k=1 means Monday, and so on.
  kday_on_or_before(k, date + 7)
}

kday_before <- function(k, date) {
  # Fixed date of the k-day before fixed date.
  # k=0 means Sunday, k=1 means Monday, and so on.
  kday_on_or_before(k, date - 1)
}

nth_kday <- function(n, k, g_date) {
  # Fixed date of n-th k-day after Gregorian date. If
  # n>0, return the n-th k-day on or after date.
  # If n<0, return the n-th k-day on or before date.
  # A k-day of 0 means Sunday, 1 means Monday, and so on.
  date <- as_rd(g_date)

  if (n > 0) {
    kday_before(k, date) + 7 * n
  } else if (n < 0) {
    kday_after(k, date) + 7 * n
  } else {
    as_rd(NA_integer_)
  }
}

first_kday <- function(k, g_date) {
  # Fixed date of first k-day on or after Gregorian date.
  # A k-day of 0 means Sunday, 1 means Monday, and so on.
  nth_kday(1, k, g_date)
}

last_kday <- function(k, g_date) {
  # Fixed date of last k-day on or before Gregorian date.
  # A k-day of 0 means Sunday, 1 means Monday, and so on.
  nth_kday(-1, k, g_date)
}
