
# Standard date accessors
standard_month <- function(date) {
  field(date, "month")
}

standard_day <- function(date) {
  field(date, "day")
}

standard_year <- function(date) {
  field(date, "year")
}


# Format date
format_date <- function(parts) {
  apply(as.data.frame(unclass(parts)), 1, function(x) {
    paste(sprintf("%.2d", x), collapse = "-")
  })
}


# Sum powers of x with coefficients in vector a
poly <- function(x, a) {
  p <- length(a)
  if (p == 0) {
    return(rep(0, length(x)))
  }
  X <- matrix(1, nrow = length(x), ncol = p + 1)
  if (p > 1) {
    for (i in seq(p)) {
      X[, i + 1] <- X[, i] * x
    }
  }
  return(c(X %*% c(1, a)))
}


# First integer greater or equal to initial such that condition holds
next_value <- function(initial, condition_func) {
  index <- initial
  while (!condition_func(index)) {
    index <- index + 1
  }
  return(index)
}

binary_search_single <- function(lo, hi, p, e) {
  # Bisection search for x in [lo, hi] such that condition 'e' holds.
  # p determines when to go left
  x <- (lo + hi) / 2
  if (p(lo, hi)) {
    return(x)
  } else if (e(x)) {
    return(binary_search(lo, x, p, e))
  } else {
    return(binary_search(x, hi, p, e))
  }
}

binary_search <- function(lo, hi, test_func, end_func) {
  # Bisection search
  mapply(
    function(l, h) {
      binary_search_single(l, h, test_func, end_func)
    },
    lo,
    hi,
    SIMPLIFY = TRUE
  )
}

in_range <- function(tee, range) {
  range[1] <= tee & tee < range[2]
}
