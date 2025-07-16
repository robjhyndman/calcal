MEAN_TROPICAL_YEAR <- 365.242189
MEAN_SIDEREAL_YEAR <- 365.25636
MEAN_SYNODIC_MONTH <- 29.530588861

SPRING <- 0
SUMMER <- 90
AUTUMN <- 180
WINTER <- 270

#' Define calendar objects
#'
#' Generate a calendar object of class "calcal". Examples of calendars
#' produced in this way include `cal_chinese`, `cal_gregorian`, `cal_hebrew`, `cal_islamic`,
#' and `cal_iso`.
#'
#' @param name Name of calendar
#' @param short_name Short name of calendar
#' @param granularities Character vector with names of granularities of calendar
#' (e.g., for the Gregorian calendar, the granularities are `year`, `month`, and `day`).
#' @param validate_granularities Function to check granularities are valid (e.g.,
#' Gregorian months should be between 1 and 12).
#' @param format Function to specify date format as a character string.
#' @param from_rd Function to convert from RD to calendar date.
#' @param to_rd Function to convert from calendar date to RD.
#' @format An object of class `calcal`
#' @examples
#' cal_gregorian
#' tibble::tibble(
#'   x = new_date(year = 2025, month = 5, day = 1:31, calendar = cal_gregorian),
#'   y = as_date(x, calendar = cal_islamic)
#' )
#' @export
new_calendar <- function(
  name,
  short_name,
  granularities,
  validate_granularities,
  format,
  from_rd,
  to_rd
) {
  structure(
    list(
      name = name,
      short_name = short_name,
      granularities = granularities,
      validate_granularities = validate_granularities,
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
    paste(granularity_names(x), collapse = ", "),
    "\n",
    sep = ""
  )
}

#' Create a new date vector or convert a date vector to a new calendar
#'
#' New dates can be calculated using `new_date()` for any calendar. Dates can be
#' converted from one calendar to another using `as_date()`. `as_date()` also works
#' with the native R `Date` class and several other classes. When applied to
#' integers, the conversion is from the RD day number (with day 1 being
#' 01-01-01 on the Gregorian calendar).
#'
#' @param date Date vector on some calendar
#' @param calendar Target calendar of class "calcal"
#' @param ... Named arguments denoting the granularities required for `calendar`.
#' @examples
#' april2025 <- new_date(year = 2025, month = 4, day = 1:30, calendar = cal_gregorian)
#' as_date(april2025, calendar = cal_iso)
#' @rdname new_date
#' @export
as_date <- function(date, calendar) {
  UseMethod("as_date")
}

#' @export
as_date.numeric <- function(date, calendar) {
  new_rdvec(trunc(date), calendar)
}

#' @export
as_date.Date <- function(date, calendar) {
  new_rdvec(
    as.numeric(date) + as.numeric(gregorian_date(1970, 1, 1)),
    calendar
  )
}

#' @export
as_date.list <- function(date, calendar) {
  gnames <- granularity_names(calendar)
  # Set up empty granularities with correct names
  if (length(date) == 0L) {
    date <- lapply(gnames, function(x) vector("integer"))
    names(date) <- gnames
  }
  # Check arguments are valid granularities
  if (any(!(names(date) %in% gnames))) {
    invalid_grans <- names(date)[!(names(date) %in% gnames)]
    stop(paste("Invalid granularities:", paste(invalid_grans, collapse = ", ")))
  }
  # Convert to integers
  lst <- lapply(date, vec_cast, to = integer())
  # Common length recycling
  lst <- lapply(lst, vec_recycle, size = max(unlist(lapply(lst, length))))
  # Check the granularities are valid
  calendar$validate_granularities(lst)
  # Convert to RD
  if (all(unlist(lapply(lst, length)) == 0L)) {
    rd <- integer()
  } else {
    rd <- calendar$to_rd(lst)
  }
  # Create the new date object
  new_rdvec(rd, calendar)
}

#' @export
as_date.rdvec <- function(date, calendar) {
  new_rdvec(trunc(vec_data(date)), calendar)
}

#' @export
as_date.character <- function(date, calendar) {
  as_date(as.Date(date), calendar)
}

#' @export
as_date.default <- function(date, calendar) {
  stop("Cannot convert to date")
}

base_granularities <- function(date) {
  attributes(date)$calendar$from_rd(date)
}

#' @export
as.list.rdvec <- function(x, ...) {
  base_granularities(x)
}

#' @export
as.data.frame.rdvec <- function(x, ...) {
  as.data.frame(base_granularities(x))
}

#' @export
as.Date.rdvec <- function(x, ...) {
  gdate <- as.data.frame(cal_gregorian$from_rd(x))
  as.Date(apply(gdate, 1, paste, collapse = "-"))
}

#' @rdname new_date
#' @export
new_date <- function(..., calendar) {
  as_date(list(...), calendar)
}

new_rdvec <- function(rd = integer(), calendar = cal_gregorian) {
  out <- new_vctr(vec_cast(rd, double()), class = "rdvec")
  class(out) <- c("rdvec", calendar$name, "vctrs_vctr")
  attr(out, "calendar") <- calendar
  out
}

#' @export
format.rdvec <- function(x, ...) {
  attributes(x)$calendar$format(x)
}

# Generic date format function. Months can take names
# Everything else is shown as an integer
# Leap years/months shown via asterisks
format_date <- function(date, month_name = NULL) {
  parts <- base_granularities(date)
  if ("month" %in% names(parts)) {
    if (!is.null(month_name)) {
      parts[["month"]] <- month_name[parts[["month"]]]
    } else {
      parts[["month"]] <- sprintf("%.2d", parts[["month"]])
    }
  }
  if ("leap_year" %in% names(parts)) {
    parts[["year"]] <- as.character(parts[["year"]])
    parts[["year"]][parts[["leap_year"]]] <- paste0(
      parts[["year"]][parts[["leap_year"]]],
      "*"
    )
  }
  if ("leap_month" %in% names(parts)) {
    parts[["month"]][parts[["leap_month"]]] <- paste0(
      parts[["month"]][parts[["leap_month"]]],
      "*"
    )
  }
  if ("leap_day" %in% names(parts)) {
    parts[["day"]] <- as.character(parts[["day"]])
    parts[["day"]][parts[["leap_day"]]] <- paste0(
      parts[["day"]][parts[["leap_day"]]],
      "*"
    )
  }
  # Drop leap year and leap Month
  parts <- parts[!(names(parts) %in% c("leap_year", "leap_month", "leap_day"))]
  for (i in seq_along(parts)) {
    if (is.numeric(parts[[i]])) {
      parts[[i]] <- sprintf("%.2d", parts[[i]])
    }
  }
  apply(as.data.frame(parts), 1, function(x) {
    paste(x, collapse = "-")
  })
}

#' @export
vec_cast.rdvec.rdvec <- function(x, to, ...) x
#' @export
vec_cast.double.rdvec <- function(x, to, ...) vec_data(x)
#' @export
vec_cast.integer.rdvec <- function(x, to, ...) vec_data(x)

#' @export
vec_cast.rdvec.double <- function(x, to, ...) {
  x
}

#' @export
vec_ptype2.rdvec.rdvec <- function(x, y, ...) {
  ## Check same calendar
  if (!identical(attributes(x)$calendar, attributes(y)$calendar)) {
    stop("Not a common calendar")
  }
  new_rdvec()
}

#' @export
vec_ptype2.rdvec.double <- function(x, y, ...) {
  new_rdvec()
}

#' @export
vec_ptype2.double.rdvec <- function(x, y, ...) {
  new_rdvec()
}

#' @export
vec_ptype2.rdvec.integer <- function(x, y, ...) {
  new_rdvec()
}

#' @export
vec_ptype2.integer.rdvec <- function(x, y, ...) {
  new_rdvec()
}

#' @export
vec_ptype_abbr.rdvec <- function(x, ...) {
  attributes(x)$calendar$short_name
}

#' @export
obj_print_header.rdvec <- function(x, ...) {
  class(x) <- c(attributes(x)$calendar$name, class(x))
  NextMethod()
}
