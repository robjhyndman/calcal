# ==============================================================================
# Basic constants and utility functions
# ==============================================================================

# Basic arithmetic utilities
amod <- function(x, y) {
  y + (x %% (-y))
}

mod3 <- function(x, a, b) {
  result <- a + ((x - a) %% (b - a))
  result[a == b] <- x[a == b]
  return(result)
}

# Search and iteration utilities
# Return first value for which condition_fn is true
# starting with initial and incrementing by one
next_value <- function(initial, condition_fn) {
  index <- initial
  cond <- condition_fn(index) & !is.na(index)
  missing <- is.na(cond) | is.na(index)
  cond[missing] <- TRUE
  while (any(!cond)) {
    j <- !cond
    index[j] <- index[j] + 1
    cond[j] <- condition_fn(index[j]) & !is.na(index[j])
    cond[is.na(cond)] <- TRUE
  }
  index[missing] <- NA_integer_
  return(index)
}

# Same as next_value but condition_fn must use full vector
next_value2 <- function(initial, condition_fn) {
  index <- initial
  cond <- condition_fn(index) & !is.na(index)
  missing <- is.na(cond) | is.na(index)
  cond[missing] <- TRUE
  while (any(!cond)) {
    j <- !cond
    index[j] <- index[j] + 1
    cond <- condition_fn(index) & !is.na(index)
    cond[is.na(cond)] <- TRUE
  }
  index[missing] <- NA_integer_
  return(index)
}

# Return last value for which condition_fn is true
# starting with initial and incrementing by one.
final_value <- function(initial, condition_fn) {
  index <- initial
  cond <- condition_fn(initial) & !is.na(index)
  missing <- is.na(cond) | is.na(index)
  cond[missing] <- FALSE
  while (any(cond)) {
    j <- cond
    index[j] <- index[j] + 1
    cond <- condition_fn(index) & !is.na(index)
    cond[is.na(cond)] <- FALSE
  }
  index[missing] <- NA_integer_
  return(index - 1)
}

binary_search_single <- function(lo, hi, test_fn, end_fn) {
  # Bisection search for x in [lo, hi] such that condition 'end_fn' holds.
  # test_fn determines when to go left
  x <- (hi + lo) / 2
  while (!end_fn(lo, hi)) {
    # Determine direction based on test function
    if (test_fn(x)) {
      hi <- x
    } else {
      lo <- x
    }
    x <- (hi + lo) / 2
  }
  return(x)
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

invert_angular <- function(f, y, a, b) {
  # Find inverse of angular function 'f' at 'y' within interval [a,b].
  # Default precision is 0.00001
  # Bisection search
  lst <- vctrs::vec_cast_common(y = y, a = a, b = b)
  output <- rep(NA_real_, length(lst$y))
  epsilon <- 1 / 100000
  miss <- is.na(lst$y) | is.na(lst$a) | is.na(lst$b)
  for (i in seq_along(output)) {
    if (!miss[i]) {
      output[i] <- binary_search(
        lo = lst$a[i],
        hi = lst$b[i],
        test_fn = function(x) {
          ((f(x) - lst$y[i]) %% 360) < 180
        },
        end_fn = function(lo, hi) {
          (hi - lo) < epsilon
        }
      )
    }
  }
  output
}


# Polynomial function with coefficients in a
# return a[1] + a[2]*x + a[3]*x^2 + ... + a[n]*x^n
poly <- function(x, a) {
  if (length(a) == 0) {
    return(0)
  }
  a[1] + x * poly(x, a[-1])
}

day_of_week_from_fixed <- function(date) {
  # The residue class of the day of the week of date
  vec_cast(date, double()) %% 7
}

fixed_from_moment <- function(tee) {
  floor(tee)
}

time_from_moment <- function(tee) {
  tee %% 1
}

from_radix <- function(a, b) {
  mult <- rev(cumprod(c(1, b)))
  if (!is.matrix(a)) {
    a <- matrix(a, nrow = 1)
  }
  apply(a, 1, function(x) sum(x * mult))
}

to_radix <- function(x, b, c = NULL) {
  if (is.null(c)) {
    if (is.null(b)) {
      return(x)
    } else {
      place_values <- rev(cumprod(c(1, b)))
      digits <- matrix(0, nrow = length(x), ncol = length(place_values))
      remainder <- x
      for (i in seq_along(place_values)) {
        digits[, i] <- remainder %/% place_values[i]
        remainder <- remainder %% place_values[i]
      }
      return(digits)
    }
  } else {
    return(to_radix(x * prod(c), c(b, c)))
  }
}

clock_from_moment <- function(tee) {
  result <- to_radix(tee, NULL, c(24, 60, 60))
  return(result[-1]) # Skip the first element
}

# Basic interval functions

in_range <- function(tee, range) {
  range[1] <= tee & tee < range[2]
}

list_range <- function(ell, range) {
  ell[in_range(ell, range)]
}

positions_in_range <- function(p, cc, cap_delta, a, b) {
  date <- mod3(p - cap_delta, a, a + cc)
  if (any(date > b)) {
    return(date[date <= b])
  } else {
    c(
      date,
      positions_in_range(p, cc, cap_delta, a + cc, b)
    )
  }
}

dates2_in_gregorian <- function(g_year, date0, date1) {
  date0 <- as_gregorian(date0)
  date1 <- as_gregorian(date1)
  out <- mapply(
    function(d0, d1, year) {
      list_range(c(d0, d1), gregorian_year_range(year))
    },
    date0,
    date1,
    g_year,
    SIMPLIFY = TRUE
  )
  c(unlist(out))
}

dates3_in_gregorian <- function(g_year, date0, date1, date2) {
  date0 <- as_gregorian(date0)
  date1 <- as_gregorian(date1)
  date2 <- as_gregorian(date2)
  out <- mapply(
    function(d0, d1, d2, year) {
      list_range(c(d0, d1, d2), gregorian_year_range(year))
    },
    date0,
    date1,
    date2,
    g_year,
    SIMPLIFY = TRUE
  )
  c(unlist(out))
}

# Julian Day functions
JD_EPOCH <- -1721424.5

moment_from_jd <- function(jd) {
  jd + JD_EPOCH
}

jd_from_moment <- function(tee) {
  tee - JD_EPOCH
}

fixed_from_jd <- function(jd) {
  floor(moment_from_jd(jd))
}

jd_from_fixed <- function(date) {
  jd_from_moment(date)
}
