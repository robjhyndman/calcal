
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

# Polynomial function with coefficients in a
# return a[1] + a[2]*x + a[3]*x^2 + ... + a[n]*x^n
poly <- function(x, a) {
  if (length(a) == 0) {
    return(0)
  }
  a[1] + x * poly(x, a[-1])
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

binary_search <- function(lo, hi, test_fn, end_fn) {
  # Bisection search
  mapply(
    function(l, h) {
      binary_search_single(l, h, test_fn, end_fn)
    },
    lo,
    hi,
    SIMPLIFY = TRUE
  )
}

invert_angular <- function(f, y, r) {
  # Find inverse of angular function 'f' at 'y' within interval [a,b].
  # Default precision is 0.00001
  # Bisection search
  lst <- vctrs::vec_cast_common(y = y, a = r[1], b = r[2])
  output <- rep(0, length(lst$y))
  epsilon <- 1/100000
  for (i in seq_along(output)) {
    output[i] <- binary_search(
      lo = lst$a[i],
      hi = lst$b[i],
      test_fn = function(x) { ((f(x) - lst$y[i]) %% 360) < 180 },
      end_fn = function(lo, hi) { (hi - lo) < epsilon }
    )
  }
  output
}

# Search and iteration utilities
next_value <- function(initial, condition_fn) {
  index <- initial
  while (!condition_fn(index)) {
    index <- index + 1
  }
  return(index)
}

final_value <- function(initial, condition_fn) {
  index <- initial
  while (condition_fn(index)) {
    index <- index + 1
  }
  return(index - 1)
}



in_range <- function(tee, range) {
  range[1] <= tee & tee < range[2]
}

list_range <- function(ell, range) {
  ell[in_range(ell, range)]
}

# Basic arithmetic utilities
amod <- function(x, y) {
  (y + (x %% y)) %% y
}

mod3 <- function(x, a, b) {
  result <-   a + ((x - a) %% (b - a))
  result[a == b] <- x[a == b]
  return(result)
}

fixed_from_moment <- function(tee) {
  floor(tee)
}

time_from_moment <- function(tee) {
  tee %% 1
}
