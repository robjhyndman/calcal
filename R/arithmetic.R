# Functions for all calendars

#' @export
#' @method vec_arith calcalvec
vec_arith.calcalvec <- function(op, x, y, ...) {
  UseMethod("vec_arith.calcalvec", y)
}
#' @export
#' @method vec_arith.calcalvec calcalvec
vec_arith.calcalvec.calcalvec <- function(op, x, y, ...) {
  vec_arith(
    op,
    attributes(x)$calendar$to_rd(x),
    attributes(y)$calendar$to_rd(y)
  )
}
#' @export
#' @method vec_arith.numeric calcalvec
vec_arith.numeric.calcalvec <- function(op, x, y, ...) {
  as_date(
    vec_arith(op, x, attributes(y)$calendar$to_rd(y)),
    calendar = attributes(y)$calendar
  )
}

#' @export
#' @method vec_arith.calcalvec numeric
vec_arith.calcalvec.numeric <- function(op, x, y, ...) {
  as_date(
    vec_arith(op, attributes(x)$calendar$to_rd(x), y),
    calendar = attributes(x)$calendar
  )
}

#' @export
as.character.calcalvec <- function(x, ...) {
  format(x)
}
