#' @title Linear Granularity
#' @description
#' Linear granularities are produced by combining the year and a cyclic granularity.
#' @param gran_fun A function that extracts a cyclic granularity from a date
#' variable. This function must return an integer or an ordered factor.
#' @param n_gran An integer giving the number of granularities per year.
#' @param name A character string giving the name of the granularity
#' @return A function that creates a linear granularity object.
#' @examples
#' # Compute quarter from a Gregorian date
#' quarter <- function(date) {
#'   factor(ceiling(month_of_year(date) / 3),
#'     levels = 1:4,
#'     labels = c("Q1", "Q2", "Q3", "Q4"),
#'     ordered = TRUE
#'   )
#' }
#' year_quarter <- linear_granularity(quarter, 4, "year-quarter")
#' year_quarter(gregorian_date(2025, 6, 25) + 1:10)
#' @export
linear_granularity <- function(gran_fun, n_gran, name) {
  function(date) {
    lin_gran(year(date), gran_fun(date), n_gran, name)
  }
}

lin_gran <- function(
    year = integer(),
    gran,
    n_gran = integer(),
    name = character()) {
  lst <- vec_recycle_common(
    year = year,
    gran = gran,
    .size = max(length(year), length(gran))
  )
  output <- new_rcrd(lst, class = "linear_granularity")
  attr(output, "n") <- n_gran
  attr(output, "name") <- name
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
