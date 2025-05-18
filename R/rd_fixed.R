#' RD fixed dates
#'
#' Create an rd_fixed object representing an RD (Rata Die) fixed date with day 1 being 01-01-01
#'
#' @param rd A numeric vector representing the number of days since (and including) 01-01-01.
#' @return An rd_fixed vector object
#' @export
#' @examples
#' rd_fixed(1:100)
rd_fixed <- function(rd = double()) {
  new_vctr(vec_cast(rd, double()), class = "rd_fixed")
}

#' @export
vec_ptype_abbr.rd_fixed <- function(x, ...) {
  "RD"
}

#' @export
vec_ptype2.rd_fixed.rd_fixed <- function(x, y, ...) rd_fixed()
#' @export
vec_ptype2.rd_fixed.double <- function(x, y, ...) rd_fixed()
#' @export
vec_ptype2.double.rd_fixed <- function(x, y, ...) rd_fixed()

#' @export
vec_cast.rd_fixed.rd_fixed <- function(x, to, ...) x
#' @export
vec_cast.rd_fixed.double <- function(x, to, ...) rd_fixed(x)
#' @export
vec_cast.double.rd_fixed <- function(x, to, ...) vec_data(x)
#' @export
vec_cast.integer.rd_fixed <- function(x, to, ...) vec_data(x)


#' Convert date to rd_fixed date
#'
#' @param date Vector of dates on some calendar
#' @param ... Additional arguments not currently used
#' @rdname rd_fixed
#' @examples
#' as_rd("2016-01-01")
#' as_rd(Sys.Date())
#' @export
as_rd <- function(date, ...) {
  UseMethod("as_rd")
}

#' @export
as_rd.default <- function(date, ...) {
  stop("cannot coerce class ", class(date), " to rd_fixed")
}

#' @export
as_rd.numeric <- function(date, ...) {
  vec_cast(date, rd_fixed())
}

#' @export
as_rd.integer <- function(date, ...) {
  vec_cast(as.double(date), rd_fixed())
}

#' @export
as_rd.Date <- function(date, ...) {
  as_rd(gregorian(
    lubridate::year(date),
    lubridate::month(date),
    lubridate::day(date)
  ))
}

#' @export
as_rd.character <- function(date, ...) {
  as_rd(as.Date(date))
}

# Arithmetic

#' @export
#' @method vec_arith rd_fixed
vec_arith.rd_fixed <- function(op, x, y, ...) {
  UseMethod("vec_arith.rd_fixed", y)
}
#' @export
vec_arith.rd_fixed.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}
#' @export
#' @method vec_arith.rd_fixed rd_fixed
vec_arith.rd_fixed.rd_fixed <- function(op, x, y, ...) {
  switch(op,
    "-" = vec_arith_base(op, x, y),
    stop_incompatible_op(op, x, y)
  )
}
#' @export
#' @method vec_arith.numeric rd_fixed
vec_arith.numeric.rd_fixed <- function(op, x, y, ...) {
  switch(op,
    "+" = rd_fixed(vec_arith_base(op, x, y)),
    "-" = rd_fixed(vec_arith_base(op, x, y)),
    stop_incompatible_op(op, x, y)
  )
}
#' @export
#' @method vec_arith.rd_fixed numeric
vec_arith.rd_fixed.numeric <- function(op, x, y, ...) {
  switch(op,
    "+" = rd_fixed(vec_arith_base(op, x, y)),
    "-" = rd_fixed(vec_arith_base(op, x, y)),
    "/" = vec_arith_base(op, x, y),
    stop_incompatible_op(op, x, y)
  )
}
