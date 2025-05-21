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
  epsilon <- 1 / 100000
  for (i in seq_along(output)) {
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

# Calendar basics
rd <- function(tee) {
  # Identity function for fixed dates/moments
  epoch <- 0
  return(tee - epoch)
}

day_of_week_from_fixed <- function(date) {
  # The residue class of the day of the week of date
  vec_cast(date, double()) %% 7
}

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

fixed_from_moment <- function(tee) {
  floor(tee)
}

time_from_moment <- function(tee) {
  tee %% 1
}


from_radix <- function(a, b, c = NULL) {
  if (is.null(c)) {
    if (is.null(b)) {
      return(a[1])
    } else {
      result <- vector(length = length(a))
      result[1] <- from_radix(a[-1], b[-length(b)]) *
        b[length(b)] +
        a[length(a)]
      return(result[1])
    }
  } else {
    total <- 0
    for (i in 1:length(a)) {
      prod_value <- 1
      for (j in 1:(i - 1)) {
        if (j <= length(b) + length(c)) {
          prod_value <- prod_value * c(b, c)[j]
        }
      }
      total <- total + a[i] * prod_value
    }
    return(total / prod(c))
  }
}

to_radix <- function(x, b, c = NULL) {
  if (is.null(c)) {
    if (is.null(b)) {
      return(x)
    } else {
      b_last <- b[length(b)]
      quot <- x %/% b_last
      rem <- x %% b_last
      return(c(to_radix(quot, b[-length(b)]), rem))
    }
  } else {
    return(to_radix(x * prod(c), c(b, c)))
  }
}

clock_from_moment <- function(tee) {
  result <- to_radix(tee, NULL, c(24, 60, 60))
  return(result[-1]) # Skip the first element
}


moment_from_unix <- function(s) {
  UNIX_EPOCH + s / (24 * 60 * 60)
}

unix_from_moment <- function(tee) {
  24 * 60 * 60 * (tee - UNIX_EPOCH)
}

# Basic interval functions

in_range <- function(tee, range) {
  range[1] <= tee & tee < range[2]
}

list_range <- function(ell, range) {
  ell[in_range(ell, range)]
}


# Format date
format_date <- function(parts) {
  # Strip out non-numeric parts
  # Currently this is only the logical indicator of a leap year in a Roman date
  # May need to update for other calendars
  parts <- unclass(parts)
  numeric_col <- unlist(lapply(parts, is.numeric))
  apply(as.data.frame(parts[numeric_col]), 1, function(x) {
    paste(sprintf("%.2d", x), collapse = "-")
  })
}
