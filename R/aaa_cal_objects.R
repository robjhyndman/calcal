#' Define calendar objects
#'
#' Generate a calendar object of class "calcal". Examples of calendars
#' produced in this way include [cal_gregorian], [cal_hebrew], [cal_islamic],
#' [cal_iso], [cal_julian], and [cal_roman].
#'
#' @param name Name of calendar
#' @param short_name Short name of calendar
#' @param epoch Epoch of calendar
#' @param granularities Granularities of calendar (e.g., for the Gregorian
#' calendar, the granularities are `year`, `month`, and `day`).
#' @param check_granularities Function to check granularities are valid (e.g.,
#' Gregorian months should be between 1 and 12).
#' @param format Functon to specify date format as a character string.
#' @param from_rd Function to convert from RD to calendar date.
#' @param to_rd Function to convert from calendar date to RD.
#' @export
cal_calendar <- function(
  name,
  short_name,
  epoch,
  granularities,
  check_granularities,
  format,
  from_rd,
  to_rd
) {
  structure(
    list(
      name = name,
      short_name = short_name,
      epoch = epoch,
      granularities = granularities,
      check_granularities = check_granularities,
      format = format,
      from_rd = from_rd,
      to_rd = to_rd
    ),
    class = "calcal"
  )
}

#' @export
print.calcal <- function(x, ...) {
  cat(
    "Calendar: ",
    x$name,
    "\n",
    "Granularities: ",
    paste(x$granularities, collapse = ", "),
    "\n",
    sep = ""
  )
}

#' Create or convert a date to a new calendar
#'
#' @param date Date vector on some calendar
#' @param calendar Target calendar of class "calcal"
#' @param ... Named arguments denoting the granularities required for `calendar`.
#' @examples
#' april25 <- new_date(year = 2025, month = 4, day = 1:30, calendar = cal_gregorian)
#' as_date(april25, calendar = cal_iso)
#' @rdname new_date
#' @export
as_date <- function(date, calendar) {
  UseMethod("as_date")
}

#' @export
as_date.numeric <- function(date, calendar) {
  new_calcalvec(date, calendar)
}

#' @export
as_date.Date <- function(date, calendar) {
  new_calcalvec(
    as.numeric(date) + gregorian_date(1970,1,1),
    calendar
  )
}

#' @export
as_date.calcalvec <- function(date, calendar) {
  new_calcalvec(vec_data(date), calendar)
}

#' @export
as_date.default <- function(date, calendar) {
  as_date(as.Date(date), calendar)
}

#' @rdname new_date
#' @export
new_date <- function(..., calendar) {
  grans <- list(...)
  # Set up empty granularities with correct names
  if (length(grans) == 0L) {
    grans <- lapply(granularities(calendar), function(x) vector("integer"))
    names(grans) <- granularities(calendar)
  }
  # Check arguments are valid granularities
  if (any(!(names(grans) %in% granularities(calendar)))) {
    invalid_grans <- names(grans)[!(names(grans) %in% granularities(calendar))]
    stop(paste("Invalid granularities:", paste(invalid_grans, collapse = ", ")))
  }
  # Convert to integers
  lst <- lapply(grans, vec_cast, to = integer())
  # Common length recycling
  lst <- lapply(lst, vec_recycle, size = max(unlist(lapply(lst, length))))
  # Check the granularities are valid
  calendar$check_granularities(lst)
  # Convert to RD
  rd <- calendar$to_rd(lst)
  # Create the new date object
  new_calcalvec(rd, calendar)
}

new_calcalvec <- function(rd=integer(), calendar=cal_gregorian) {
  out <- new_vctr(vec_cast(rd, double()), class = "calcalvec")
  attr(out, "calendar") <- calendar
  out
}

#' @export
format.calcalvec <- function(x, ...) {
  attributes(x)$calendar$format(x)
}

# Generic date format function
format_date <- function(parts) {
  # Strip out non-numeric parts
  # Currently this is only the logical indicator of a leap year in a Roman date
  # May need to update for other calendars
  parts <- unclass(parts)
  numeric_col <- unlist(lapply(parts, is.numeric))
  apply(as.data.frame(parts[numeric_col]), 1, function(x) {
    paste(sprintf("%.2d", x), collapse = "-")
  })
}


#' @export
vec_cast.calcalvec.calcalvec <- function(x, to, ...) x
#' @export
vec_cast.double.calcalvec <- function(x, to, ...) vec_data(x)
#' @export
vec_cast.integer.calcalvec <- function(x, to, ...) vec_data(x)

#' @export
vec_cast.calcalvec.double <- function(x, to, ...) {
  x
}

#' @export
vec_ptype2.calcalvec.calcalvec <- function(x, y, ...) {
  ## Check same calendar
  #if (!identical(attributes(x)$calendar, attributes(y)$calendar)) {
  #  stop("Not a common calendar")
  #}
  new_calcalvec()
}

#' @export
vec_ptype2.calcalvec.double <- function(x, y, ...) {
  new_calcalvec()
}

#' @export
vec_ptype2.double.calcalvec <- function(x, y, ...) {
  new_calcalvec()
}


#' @export
vec_ptype_abbr.calcalvec <- function(x, ...) {
  attributes(x)$calendar$short_name
}

#' @export
obj_print_header.calcalvec <- function(x, ...) {
  class(x) <- c(attributes(x)$calendar$name, class(x))
  NextMethod()
}

#' @export
as.Date.calcalvec <- function(x, ...) {
  gdate <- cal_gregorian$from_rd(x)
  as.Date(apply(as.data.frame(gdate), 1, paste, collapse = "-"))
}
