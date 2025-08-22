#' Compute granularities from dates
#'
#' Compute days, weeks, or months from a vector of dates. These work for Gregorian
#' dates, and for some other calendars where it makes sense. In particular, `day_of_week`
#' has been implemented for many calendars that contain the concept of a week. Similarly,
#' `day_of_month`, `day_of_year` and `days_remaining` will work for several calendars.
#'
#' @details
#' \code{week_of_year()} returns the ISO 8601 week number with \code{first_day} as Monday.
#' Under this standard, week 1 of a year is defined as the first week with at least 4 days in the year;
#' equivalently, it is the week  containing 4 January. There is no week 0; instead week 1 of a year may
#' begin in the previous calendar year.
#'
#' \code{week_of_month()} is defined analogously where week 1 of a month is the first week with at least
#' 4 days in the month; equivalently, it is the week containing the 4th day of the month. There is no week 0;
#' instead week 1 of a month may begin in the previous calendar month.
#'
#' \code{days_remaining()} returns the number of days remaining in the year.
#'
#' Other functions should be self-explanatory.
#'
#' @param date A vector of dates
#' @param first_day Character denoting first day of the week. Default is \code{"Monday"}
#' @param ... Other arguments used for specific calendars
#' @return A vector of numerical values for the requested granularity. In the case of `day_of_week()`, it returns a character vector of the name of the day of the week, or a numeric vector if `numeric = TRUE` is specified.
#' @examples
#' april2025 <- gregorian_date(2025, 4, 1:30)
#' day_of_week(april2025)
#' day_of_month(april2025)
#' day_of_year(april2025)
#' days_remaining(april2025)
#' week_of_month(april2025)
#' week_of_year(april2025)
#' month_of_year(april2025)
#' @rdname gregorian-parts
#' @export
day_of_week <- function(date, ...) {
  UseMethod("day_of_week")
}

#' @export
day_of_week.default <- function(date, ...) {
  day_of_week(as_gregorian(date), ...)
}

#' @export
day_of_week.gregorian <- function(
  date,
  numeric = FALSE,
  first_day = "Monday",
  abbreviate = FALSE,
  ...
) {
  dow <- day_of_week_from_fixed(date) + 1
  if (numeric) {
    first_day <- pmatch(
      first_day,
      c(
        "Sunday",
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday"
      )
    )
    if (is.na(first_day)) {
      stop("I can't determine the first day of the week")
    }
    return((dow - first_day) %% 7 + 1)
  } else {
    if (abbreviate) {
      return(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[dow])
    } else {
      return(c(
        "Sunday",
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday"
      )[dow])
    }
  }
}

#' @rdname gregorian-parts
#' @export
day_of_month <- function(date) {
  if (!("month" %in% granularity_names(date))) {
    stop("Date must contain months")
  }
  granularity(date, "day")
}

#' @rdname gregorian-parts
#' @export
# Day number in year of Gregorian date date
# Called day_number in CC book and code
# Rewritten to work for any calendar with year granularity
day_of_year <- function(date) {
  gran <- granularity_names(date)
  if (!("year" %in% gran)) {
    stop("Date must contain years")
  }
  date0 <- base_granularities(date)
  for (f in gran[gran != "year"]) {
    date0[[f]] <- rep(1, length(date))
  }
  date0 <- get_calendar(date)$to_rd(date0)
  as.numeric(date - date0 + 1)
}

#' @rdname gregorian-parts
#' @export
days_remaining <- function(date) {
  gran <- granularity_names(date)
  if (!("year" %in% gran)) {
    stop("Date must contain years")
  }
  date0 <- base_granularities(date)
  date0[["year"]] <- date0[["year"]] + 1
  for (f in gran[gran != "year"]) {
    date0[[f]] <- rep(1, length(date))
  }
  date0 <- get_calendar(date)$to_rd(date0)
  as.numeric(date0 - date - 1)
}

#' @rdname gregorian-parts
#' @export
week_of_month <- function(date, first_day = "Monday") {
  dow <- day_of_week(date, numeric = TRUE, first_day = first_day)
  date <- date + (4 - dow)
  day1 <- gregorian_date(
    granularity(date, "year"),
    granularity(date, "month"),
    1
  )
  (date - day1) %/% 7 + 1
}

#' @rdname gregorian-parts
#' @export
week_of_year <- function(date, first_day = "Monday") {
  dow <- day_of_week(date, numeric = TRUE, first_day = first_day)
  date <- date + (4 - dow)
  jan1 <- gregorian_date(granularity(date, "year"), JANUARY, 1)
  (as_gregorian(date) - jan1) %/% 7 + 1
}

#' @rdname gregorian-parts
#' @export
month_of_year <- function(date) {
  granularity(date, "month")
}

#' @rdname gregorian-parts
#' @export
year <- function(date) {
  granularity(date, "year")
}
