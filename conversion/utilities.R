# Calendrica 4.0 -- R
# R conversion of "Calendrical Calculations" code by E. M. Reingold and N. Dershowitz
# Original Lisp code copyright by the authors

#==============================================================================
# Basic constants and utility functions
#==============================================================================

BOGUS <- "bogus" # Used to denote nonexistent dates

# Basic arithmetic utilities
amod <- function(x, y) {
  (y + (x %% y)) %% y
}

mod3 <- function(x, a, b) {
  if (a == b) {
    return(x)
  }
  a + ((x - a) %% (b - a))
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

sum_over <- function(expression_fn, initial, condition_fn) {
  index <- initial
  total <- 0
  while (condition_fn(index)) {
    total <- total + expression_fn(index)
    index <- index + 1
  }
  return(total)
}

prod_over <- function(expression_fn, initial, condition_fn) {
  index <- initial
  product <- 1
  while (condition_fn(index)) {
    product <- product * expression_fn(index)
    index <- index + 1
  }
  return(product)
}

binary_search <- function(lo, hi, test_fn, end_fn) {
  repeat {
    x <- (lo + hi) / 2
    left <- test_fn(x)
    if (left) {
      hi <- x
    } else {
      lo <- x
    }
    if (end_fn(lo, hi)) {
      return((lo + hi) / 2)
    }
  }
}

invert_angular <- function(f, y, r) {
  epsilon <- 1 / 100000
  binary_search(
    lo = r[1],
    hi = r[2],
    test_fn = function(x) ((f(x) - y) %% 360) < 180,
    end_fn = function(lo, hi) (hi - lo) < epsilon
  )
}

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

# Day of week constants
SUNDAY <- 0
MONDAY <- 1
TUESDAY <- 2
WEDNESDAY <- 3
THURSDAY <- 4
FRIDAY <- 5
SATURDAY <- 6

day_of_week_from_fixed <- function(date) {
  (date - rd(0) - SUNDAY) %% 7
}

# Standard date accessors
standard_month <- function(date) {
  date[2]
}

standard_day <- function(date) {
  date[3]
}

standard_year <- function(date) {
  date[1]
}

# Time functions
time_of_day <- function(hour, minute, second) {
  c(hour, minute, second)
}

hour <- function(clock) {
  clock[1]
}

minute <- function(clock) {
  clock[2]
}

seconds <- function(clock) {
  clock[3]
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
      return(list(x))
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

time_from_clock <- function(hms) {
  from_radix(hms, NULL, c(24, 60, 60)) / 24
}

# Julian Day functions
JD_EPOCH <- rd(-1721424.5)

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

MJD_EPOCH <- rd(678576)

fixed_from_mjd <- function(mjd) {
  mjd + MJD_EPOCH
}

mjd_from_fixed <- function(date) {
  date - MJD_EPOCH
}

UNIX_EPOCH <- rd(719163)

moment_from_unix <- function(s) {
  UNIX_EPOCH + s / (24 * 60 * 60)
}

unix_from_moment <- function(tee) {
  24 * 60 * 60 * (tee - UNIX_EPOCH)
}

# Basic interval functions
interval <- function(t0, t1) {
  c(t0, t1)
}

interval_closed <- function(t0, t1) {
  c(t0, t1)
}

begin <- function(range) {
  range[1]
}

end <- function(range) {
  range[2]
}

in_range <- function(tee, range) {
  begin(range) <= tee && tee < end(range)
}

list_range <- function(ell, range) {
  result <- c()
  for (t in ell) {
    if (in_range(t, range)) {
      result <- c(result, t)
    }
  }
  return(result)
}

positions_in_range <- function(p, c, cap_delta, range) {
  a <- begin(range)
  b <- end(range)
  date <- mod3(p - cap_delta, a, a + c)

  if (date >= b) {
    return(c())
  } else {
    return(c(date, positions_in_range(p, c, cap_delta, interval(a + c, b))))
  }
}
