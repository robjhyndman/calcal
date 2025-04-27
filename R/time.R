#' Time of day
#'
#' Create a time object
#'
#' @param hour A numeric vector of hours
#' @param minute A numeric vector of minutes
#' @param second A numeric vector of seconds
#' @return A time vector object
#' @export
time <- function(
  hour = integer(),
  minute = integer(),
  second = numeric(),
  zone = numeric()
) {
  lst <- vec_cast_common(
    hour = hour,
    minute = minute,
    second = second,
    .to = numeric()
  )
  lst <- vec_recycle_common(
    hour = lst$hour,
    minute = lst$minute,
    second = lst$second
  )
  check_time(lst)
  new_rcrd(lst, class = "time")
}

check_time <- function(args) {
  hour <- args$hour
  minute <- args$minute
  second <- args$second
  if (hour < 0 | hour > 23) {
    stop("hour must be between 0 and 23")
  }
  if (minute < 0 | minute > 59) {
    stop("minute must be between 0 and 23")
  }
  if (second < 0 | second >= 60) {
    stop("second must be between 0 and 23")
  }
}

#' Convert to time of day
#'
#' @param x Vector of times
#' @param ... Additional arguments not currently used
#' @rdname time
#' @examples
#' as_time(Sys.time())
#' @export
as_time <- function(x, ...) {
  UseMethod("as_time")
}

#' @export
as_time.default <- function(x, ...) {
  stop("cannot coerce class ", class(x), " to time")
}

# Convert partial day to hours, minutes, seconds
#' @export
as_time.numeric <- function(x, ...) {
  x <- 24 * (x %% 1)
  h <- trunc(x)
  m <- trunc((x - h) * 60)
  s <- (x - h - m / 60) * 3600
  c(h, m, s)
}

#' @export
as_time.rd_fixed <- function(x, ...) {
  as_time(vec_data(x))
}

#' @export
as_time.POSIXct <- function(x, ...) {
  as_time(as.POSIXlt(x))
}

#' @export
as_time.POSIXlt <- function(x, ...) {
  c(x$hour, x$min, x$sec)
}
