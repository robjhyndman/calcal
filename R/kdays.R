kday_on_or_before <- function(date, k) {
  # Fixed date of the k-day on or before fixed date.
  # k=0 means Sunday, k=1 means Monday, and so on.
  date - day_of_week_from_fixed(date - k)
}

kday_on_or_after <- function(date, k) {
  # Fixed date of the k-day on or after fixed date.
  # k=0 means Sunday, k=1 means Monday, and so on.
  kday_on_or_before(date + 6, k)
}

kday_nearest <- function(date, k) {
  # Fixed date of the k-day nearest fixed date.
  # k=0 means Sunday, k=1 means Monday, and so on.
  kday_on_or_before(date + 3, k)
}

kday_after <- function(date, k) {
  # Fixed date of the k-day after fixed date.
  # k=0 means Sunday, k=1 means Monday, and so on.
  kday_on_or_before(date + 7, k)
}

kday_before <- function(date, k) {
  # Fixed date of the k-day before fixed date.
  # k=0 means Sunday, k=1 means Monday, and so on.
  kday_on_or_before(date - 1, k)
}

nth_kday <- function(n, k, g_date) {
  # Fixed date of n-th k-day after Gregorian date. If
  # n>0, return the n-th k-day on or after date.
  # If n<0, return the n-th k-day on or before date.
  # A k-day of 0 means Sunday, 1 means Monday, and so on.
  date <- as_rd(g_date)

  if (n > 0) {
    return(7 * n + kday_before(date, k))
  } else {
    return(7 * n + kday_after(date, k))
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
