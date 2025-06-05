# Functions for all calendars

#' @export
#' @method vec_arith calcalvec
vec_arith.calcalvec <- function(op, x, y, ...) {
  UseMethod("vec_arith.calcalvec", y)
}
#' @export
#' @method vec_arith.calcalvec calcalvec
vec_arith.calcalvec.calcalvec <- function(op, x, y, ...) {
  vec_arith(op, vec_data(x), vec_data(y))
}
#' @export
#' @method vec_arith.numeric calcalvec
vec_arith.numeric.calcalvec <- function(op, x, y, ...) {
  new_calcalvec(
    vec_arith(op, x, vec_data(y)),
    attributes(y)$calendar
  )
}

#' @export
#' @method vec_arith.calcalvec numeric
vec_arith.calcalvec.numeric <- function(op, x, y, ...) {
  new_calcalvec(
    vec_arith(op, vec_data(x), y),
    attributes(x)$calendar
  )
}

#' @export
as.character.calcalvec <- function(x, ...) {
  format(x)
}
