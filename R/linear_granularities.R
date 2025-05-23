#' @title Create Linear-Cyclic Granularity Combinations
#' @description
#' Linear-cyclic granularity combinations are produced by combining two
#' granularities, one nested within the other. For example, the year and the
#' month, or the week and the day-of-week. It is assumed that the first
#' granularity is linear (i.e., it increases over time), and the second
#' granularity is cycli, nested within the first granularity, and possibly
#' periodic. Both month-of-year and day-of-week are cyclic and periodic
#' granularities. But day-of-month is cyclic but not periodic, because the
#' number of days in a month is not constant.
#' @param gran_fun1 A function that extracts a linear granularity from a date.
#' This function must return an integer.
#' @param gran_fun2 A function that extracts a cyclic granularity from a date, nested within `gran_fun1`.
#' This function must return an integer.
#' @param gran_levels A character vector of the granularity levels for `gran_fun2`.
#' @param gran_name A character string giving the name of the linear granularity.
#' @param periodic A logical vector indicating if the second (cyclic) granularity is periodic.
#' @return A function that creates a linear granularity object.
#' @examples
#' # Year-quarter granularity
#' ## Compute quarter from a Gregorian date
#' quarter <- function(date) {
#'   ceiling(month_of_year(date) / 3)
#' }
#' year_quarter <- linear_cyclic(year, quarter, paste0("Q", 1:4), "year-quarter")
#' year_quarter(gregorian_date(2025, 6, 25) + 1:10)
#'
#' # Week-day granularity
#' ## Compute week as linear granularity
#' week <- function(date) {
#'   (seq_along(date) - 1) %/% 7 + 1
#' }
#' ## Compute day as numerical cyclic granularity
#' day <- function(date) {
#'   day_of_week(date, numeric = TRUE)
#' }
#' week_day <- linear_cyclic(
#'   week,
#'   day,
#'   c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
#'   gran_name = "week-day"
#' )
#' week_day(gregorian_date(2025, 6, 25) + 1:10)
#' @export
linear_cyclic <- function(
    gran_fun1,
    gran_fun2,
    gran_levels,
    gran_name,
    periodic = TRUE) {
  function(date) {
    lin_cyc_gran(
      gran_fun1(date),
      gran_fun2(date),
      gran_levels,
      gran_name,
      periodic
    )
  }
}

lin_cyc_gran <- function(
    gran1 = integer(),
    gran2 = integer(),
    gran_levels = character(),
    gran_name = character(),
    periodic = logical()) {
  lst <- vec_recycle_common(
    gran1 = gran1,
    gran2 = factor(
      gran2,
      levels = seq_along(gran_levels),
      labels = gran_levels,
      ordered = TRUE
    ),
    .size = max(length(gran1), length(gran2))
  )
  if (any(diff(lst$gran1) < 0)) {
    stop("gran1 must be strictly increasing")
  }
  if (any(gran2 > length(gran_levels))) {
    stop("gran2 must be in 1:", length(gran_levels))
  }
  output <- new_rcrd(lst, class = "linear_cyclic")
  attr(output, "name") <- gran_name
  attr(output, "periodic") <- periodic
  return(output)
}

#' @export
format.linear_cyclic <- function(x, ...) {
  paste0(field(x, "gran1"), "-", field(x, "gran2"))
}

#' @export
as.character.linear_cyclic <- function(x, ...) {
  format(x, ...)
}

#' @export
vec_ptype_full.linear_cyclic <- function(x, ...) {
  attributes(x)$name
}

#' @export
vec_ptype2.linear_cyclic.linear_cyclic <- function(x, y, ...) {
  lin_cyc_gran()
}

#' @export
vec_cast.linear_cyclic.linear_cyclic <- function(x, to, ...) x
#' @export
vec_cast.double.linear_cyclic <- function(x, to, ...) {
  if (attr(x, "periodic")) {
    field(x, "gran1") +
      as.numeric(field(x, "gran2")) / length(levels(field(x, "gran2")))
  } else {
    stop("Cannot cast an aperiodic granularity to numeric")
  }
}

# Arithmetic

#' @export
#' @method vec_arith linear_cyclic
vec_arith.linear_cyclic <- function(op, x, y, ...) {
  UseMethod("vec_arith.linear_cyclic", y)
}

#' @export
#' @method vec_arith.linear_cyclic linear_cyclic
vec_arith.linear_cyclic.linear_cyclic <- function(op, x, y, ...) {
  switch(op,
    "-" = vec_arith_base(op, x, y),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.numeric linear_cyclic
vec_arith.numeric.linear_cyclic <- function(op, x, y, ...) {
  vec_arith(op, y, x)
}

#' @export
#' @method vec_arith.linear_cyclic numeric
vec_arith.linear_cyclic.numeric <- function(op, x, y, ...) {
  if (!attr(x, "periodic")) {
    stop("Cannot add a numeric to an aperiodic granularity")
  }
  nlevels <- length(levels(field(x, "gran2")))
  new_gran <- switch(op,
    "+" = vec_arith_base(op, field(x, "gran2"), y),
    "-" = vec_arith_base(op, field(x, "gran2"), y),
    stop_incompatible_op(op, x, y)
  )
  field(x, "gran1") <- field(x, "gran1") + (new_gran - 1) %/% nlevels
  field(x, "gran2") <- factor(
    (new_gran - 1) %% nlevels + 1,
    levels = seq_along(levels(field(x, "gran2"))),
    labels = levels(field(x, "gran2")),
    ordered = TRUE
  )
  return(x)
}
