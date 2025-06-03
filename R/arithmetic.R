# Functions for all calendars

#' @export
#' @method vec_arith calcalcal
vec_arith.calcalcal <- function(op, x, y, ...) {
  UseMethod("vec_arith.calcalcal", y)
}
#' @export
#' @method vec_arith.calcalcal calcalcal
vec_arith.calcalcal.calcalcal <- function(op, x, y, ...) {
  vec_arith(op, as_rd(x), as_rd(y))
}
#' @export
#' @method vec_arith.numeric calcalcal
vec_arith.numeric.calcalcal <- function(op, x, y, ...) {
  do.call(
    paste0("as_", class(y)[1]),
    list(vec_arith(op, x, as_rd(y)))
  )
}
#' @export
#' @method vec_arith.calcalcal numeric
vec_arith.calcalcal.numeric <- function(op, x, y, ...) {
  do.call(
    paste0("as_", class(x)[1]),
    list(vec_arith(op, as_rd(x), y))
  )
}

#' @export
as.character.calcalcal <- function(x, ...) {
  format(x)
}
