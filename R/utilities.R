adjusted_mod <- function(x, y) {
  # The value of (x mod y) with y instead of 0
  y + (x %% y)
}

# Arguments past as a list to these functions

all_equal_length <- function(args) {
  # Check if all arguments are of equal length
  lengths <- unlist(lapply(args, length))
  all(lengths == lengths[1])
}

all_numeric <- function(args) {
  # Check if all arguments are numeric
  all(unlist(lapply(args, is.numeric)))
}

all_integer <- function(args) {
  # Check if all arguments are integer
  all(unlist(lapply(
    args,
    function(x) all(abs(x - round(x)) < 1e-10, na.rm = TRUE)
  )))
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
  if(p == 0) {
    return(rep(0, length(x)))
  }
  X <- matrix(1, nrow = length(x), ncol=p+1)
  if(p > 1) {
    for(i in seq(p)) {
      X[,i+1] <- X[,i] * x
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

binary_search_single <- function(lo, hi, test_func, end_func) {
  # Bisection search
  l <- lo
  h <- hi
  x <- FALSE

  repeat {
    x <- (l + h) / 2
    left <- test_func(x)
    if (left) {
      l <- l
      h <- x
    } else {
      l <- x
      h <- h
    }

    if (end_func(l, h)) {
      return((l + h) / 2)
    }
  }
}

binary_search <- function(lo, hi, test_func, end_func) {
  # Bisection search
  mapply(function(l, h) {
    binary_search_single(l, h, test_func, end_func)
  }, lo, hi, SIMPLIFY = TRUE)
}
