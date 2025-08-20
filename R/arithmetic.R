# Functions for all calendars

#' @export
#' @method vec_arith rdvec
vec_arith.rdvec <- function(op, x, y, ...) {
  UseMethod("vec_arith.rdvec", y)
}
#' @export
#' @method vec_arith.rdvec rdvec
vec_arith.rdvec.rdvec <- function(op, x, y, ...) {
  vec_arith(op, vec_data(x), vec_data(y))
}
#' @export
#' @method vec_arith.numeric rdvec
vec_arith.numeric.rdvec <- function(op, x, y, ...) {
  new_rdvec(
    vec_arith(op, x, vec_data(y)),
    get_calendar(y)
  )
}

#' @export
#' @method vec_arith.rdvec numeric
vec_arith.rdvec.numeric <- function(op, x, y, ...) {
  new_rdvec(
    vec_arith(op, vec_data(x), y),
    get_calendar(x)
  )
}

#' @export
as.character.rdvec <- function(x, ...) {
  format(x)
}
