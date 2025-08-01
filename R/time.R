# Time of day
MORNING <- TRUE
EVENING <- FALSE

#' Time of day
#'
#' Create a time object
#'
#' @param hour A numeric vector of hours
#' @param minute A numeric vector of minutes
#' @param second A numeric vector of seconds
#' @return A time_of_day vector object, stored as a vctrs record containing hours, minutes and seconds.
#' @export
time_of_day <- function(
  hour = integer(),
  minute = integer(),
  second = numeric()
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
    second = lst$second,
    .size = max(unlist(lapply(lst, length)))
  )
  validate_time(lst)
  # Convert NaN to NA
  lst$hour[is.nan(lst$hour)] <- NA_integer_
  lst$minute[is.nan(lst$minute)] <- NA_integer_
  lst$second[is.nan(lst$second)] <- NA_real_

  new_rcrd(lst, class = "time_of_day")
}

validate_time <- function(args) {
  hour <- args$hour
  minute <- args$minute
  second <- args$second
  if (any(hour < 0 | hour > 23, na.rm = TRUE)) {
    stop("hour must be between 0 and 23")
  }
  if (any(minute < 0 | minute > 59, na.rm = TRUE)) {
    stop("minute must be between 0 and 23")
  }
  if (any(second < 0 | second >= 60, na.rm = TRUE)) {
    stop("second must be between 0 and 23")
  }
}

#' @export
format.time_of_day <- function(x, ...) {
  sprintf(
    "%.2d:%.2d:%.2f",
    field(x, "hour"),
    field(x, "minute"),
    field(x, "second")
  )
}

#' Convert to time of day
#'
#' @param x Vector of times
#' @param ... Additional arguments not currently used
#' @return A vector containing "time_of_day" objects
#' @seealso [time_of_day]
#' @rdname time
#' @examples
#' as_time_of_day(Sys.time())
#' @export
as_time_of_day <- function(x, ...) {
  UseMethod("as_time_of_day")
}

#' @export
as_time_of_day.default <- function(x, ...) {
  stop("cannot coerce class ", class(x), " to time")
}

# Interpret x as days.
# Convert final partial day to hours, minutes, seconds
#' @export
as_time_of_day.numeric <- function(x, ...) {
  x <- 24 * (x %% 1)
  h <- trunc(x)
  m <- trunc((x - h) * 60)
  s <- (x - h - m / 60) * 3600
  time_of_day(h, m, s)
}

#' @export
as_time_of_day.rdvec <- function(x, ...) {
  as_time_of_day(vec_data(x))
}

#' @export
as_time_of_day.POSIXct <- function(x, ...) {
  as_time_of_day(as.POSIXlt(x))
}

#' @export
as_time_of_day.POSIXlt <- function(x, ...) {
  time_of_day(x$hour, x$min, x$sec)
}

#' @export
vec_ptype2.time_of_day.time_of_day <- function(x, y, ...) time_of_day()

#' @export
vec_cast.double.time_of_day <- function(x, ...) {
  hour <- field(x, "hour")
  minute <- field(x, "minute")
  second <- field(x, "second")
  hour + minute / 60 + second / 3600
}

#' @export
#' @method vec_arith time_of_day
vec_arith.time_of_day <- function(op, x, y, ...) {
  UseMethod("vec_arith.time_of_day", y)
}


#' @export
#' @method vec_arith.time_of_day time_of_day
vec_arith.time_of_day.time_of_day <- function(op, x, y, ...) {
  vec_arith(op, as.numeric(x), as.numeric(y))
}

#' @export
#' @method vec_arith.time_of_day numeric
vec_arith.time_of_day.numeric <- function(op, x, y, ...) {
  vec_arith(op, as.numeric(x), as.numeric(y))
}

#' @export
#' @method vec_arith.numeric time_of_day
vec_arith.numeric.time_of_day <- function(op, x, y, ...) {
  vec_arith(op, as.numeric(x), as.numeric(y))
}
