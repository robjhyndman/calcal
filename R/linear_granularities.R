#' @title Linear Granularity
#' @description
#' Linear granularities are produced by combining the year and a cyclic granularity.
#' @param gran_fun A function that extracts a cyclic granularity from a date
#' variable. This function must return an integer.
#' @param gran_levels A character vector of the granularity levels.
#' @param gran_name A character string giving the name of the granularity
#' @return A function that creates a linear granularity object.
#' @examples
#' # Compute quarter from a Gregorian date
#' quarter <- function(date) {
#'   ceiling(month_of_year(date) / 3)
#' }
#' year_quarter <- linear_granularity(quarter, paste0("Q", 1:4), "year-quarter")
#' year_quarter(gregorian_date(2025, 6, 25) + 1:10)
#' @export
linear_granularity <- function(gran_fun, gran_levels, gran_name) {
  function(date) {
    lin_gran(year(date), gran_fun(date), gran_levels, gran_name)
  }
}

lin_gran <- function(
    year = integer(),
    gran = integer(),
    gran_levels = character(),
    gran_name = character()) {
  lst <- vec_recycle_common(
    year = year,
    gran = factor(gran, levels = seq_along(gran_levels), labels = gran_levels, ordered = TRUE),
    .size = max(length(year), length(gran))
  )
  output <- new_rcrd(lst, class = "linear_granularity")
  attr(output, "name") <- gran_name
  return(output)
}

#' @export
format.linear_granularity <- function(x, ...) {
  paste0(field(x, "year"), "-", field(x, "gran"))
}

#' @export
as.character.linear_granularity <- function(x, ...) {
  format(x, ...)
}

#' @export
vec_ptype_full.linear_granularity <- function(x, ...) {
  attributes(x)$name
}

#' @export
vec_ptype2.linear_granularity.linear_granularity <- function(x, y, ...) {
  lin_gran()
}

#' @export
vec_cast.linear_granularity.linear_granularity <- function(x, to, ...) x
#' @export
vec_cast.double.linear_granularity <- function(x, to, ...) {
  field(x, "year") +
    as.numeric(field(x, "gran")) / length(levels(field(x, "gran")))
}

# Arithmetic

#' @export
#' @method vec_arith linear_granularity
vec_arith.linear_granularity <- function(op, x, y, ...) {
  UseMethod("vec_arith.linear_granularity", y)
}

#' @export
#' @method vec_arith.linear_granularity linear_granularity
vec_arith.linear_granularity.linear_granularity <- function(op, x, y, ...) {
  switch(op,
    "-" = vec_arith_base(op, x, y),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.numeric linear_granularity
vec_arith.numeric.linear_granularity <- function(op, x, y, ...) {
  vec_arith(op, y, x)
}

#' @export
#' @method vec_arith.linear_granularity numeric
vec_arith.linear_granularity.numeric <- function(op, x, y, ...) {
  nlevels <- length(levels(field(x, "gran")))
  new_gran <- switch(op,
    "+" = vec_arith_base(op, field(x, "gran"), y),
    "-" = vec_arith_base(op, field(x, "gran"), y),
    stop_incompatible_op(op, x, y)
  )
  field(x, "year") <- field(x, "year") + (new_gran - 1) %/% nlevels
  field(x, "gran") <- factor(
    (new_gran - 1) %% nlevels + 1,
    levels = seq_along(levels(field(x, "gran"))),
    labels = levels(field(x, "gran")),
    ordered = TRUE
  )
  return(x)
}
