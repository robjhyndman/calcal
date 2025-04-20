#' @importFrom S7 class_any class_numeric class_character class_Date new_class new_generic method `method<-`

# A class representing an RD fixed date with day 1 being 01-01-01
rd_fixed <- S7::new_class(
  "rd_fixed",
  properties = list(
    date = class_numeric
  ),
  validator = function(self) {
    if (!is.numeric(self@date)) {
      "@dates must be numeric values"
    } else if (any(abs(self@date - round(self@date)) > 1e-10)) {
      "@dates must be an integer"
    }
  }
)

#' Create a new rd_fixed object
#'
#' @param date The numeric date representing the RD fixed date
#' @return An rd_fixed object
new_rd_fixed <- function(date) {
  rd_fixed(date = date)
}

# Register print method for rd_fixed class
method(print, rd_fixed) <- function(x, ...) {
  paste("RD", x@date) |> print()
}

# Arithmetic operations
method(`+`, list(rd_fixed, class_numeric)) <- function(e1, e2) {
  as_rd(e1@date + e2)
}
method(`+`, list(class_numeric, rd_fixed)) <- function(e1, e2) {
  as_rd(e1 + e2@date)
}
method(`-`, list(rd_fixed, class_numeric)) <- function(e1, e2) {
  as_rd(e1@date - e2)
}
method(`-`, list(class_numeric, rd_fixed)) <- function(e1, e2) {
  as_rd(e1 - e2@date)
}
method(`-`, list(rd_fixed, rd_fixed)) <- function(e1, e2) {
  e1@date - e2@date
}

# GT and LT
method(`>`, list(rd_fixed, rd_fixed)) <- function(e1, e2) {
  e1@date > e2@date
}
method(`>=`, list(rd_fixed, rd_fixed)) <- function(e1, e2) {
  e1@date >= e2@date
}
method(`<`, list(rd_fixed, rd_fixed)) <- function(e1, e2) {
  e1@date < e2@date
}
method(`<=`, list(rd_fixed, rd_fixed)) <- function(e1, e2) {
  e1@date <= e2@date
}
method(`==`, list(rd_fixed, rd_fixed)) <- function(e1, e2) {
  e1@date == e2@date
}

#' Convert date to rd_fixed date
#'
#' @param date Date on some calendar
#' @param ... Additional arguments
#' @return An rd_fixed object representing RD fixed date
#' @examples
#' as_rd("2016-01-01")
#' as_rd(Sys.Date())
#' @export
as_rd <- new_generic("as_rd", "date")

# Method for numeric object
method(as_rd, class_numeric) <- function(date, ...) {
  new_rd_fixed(date)
}

# Methods for Date objects
#' @importFrom lubridate year month day
method(as_rd, class_Date) <- function(date, ...) {
  as_rd(gregorian_date(
    lubridate::year(date),
    lubridate::month(date),
    lubridate::day(date)
  ))
}

method(as.Date, rd_fixed) <- function(x, ...) {
  as.Date(x@date, origin = "01-01-01") - 1
}

method(as_rd, class_any) <- function(date, ...) {
  as_rd(as.Date(date))
}
