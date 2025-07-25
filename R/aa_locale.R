#' Locations
#'
#' Create a location object. These are used for calculating the timing of
#' astronomical events such as sunrise and sunset.
#'
#' @param latitude A numeric vector of latitudes
#' @param longitude A numeric vector of longitudes
#' @param elevation A numeric vector of elevations above sea level (in metres)
#' @param zone A numeric vector of time zones (in hours, relative to UTC)
#' @return A location vector object
#' @examples
#' melbourne <- location(-37.8136, 144.9631, 31, 10)
#' sunrise("2025-01-01", melbourne)
#' @export
# Differs from original code as zone is in hours, not days
location <- function(
  latitude = numeric(),
  longitude = numeric(),
  elevation = numeric(),
  zone = numeric()
) {
  lst <- vec_cast_common(
    latitude = latitude,
    longitude = longitude,
    elevation = elevation,
    zone = zone,
    .to = numeric()
  )
  lst <- vec_recycle_common(
    latitude = lst$latitude,
    longitude = lst$longitude,
    elevation = lst$elevation,
    zone = lst$zone,
    .size = max(unlist(lapply(lst, length)))
  )
  validate_locale(lst)
  new_rcrd(lst, class = "location")
}

validate_locale <- function(args) {
  latitude <- args$latitude
  longitude <- args$longitude
  elevation <- args$elevation
  zone <- args$zone
  if (any(zone < -12 | zone > 14, na.rm = TRUE)) {
    stop("zone must be between -12 and 14")
  }
  if (any(latitude < -90 | latitude > 90, na.rm = TRUE)) {
    stop("latitude must be between -90 and 90")
  }
  if (any(longitude < -180 | longitude > 180, na.rm = TRUE)) {
    stop("longitude must be between -180 and 180")
  }
  if (any(elevation < -420, na.rm = TRUE)) {
    stop("Lowest point on earth is 420m below sea level")
  }
  if (any(elevation > 8848, na.rm = TRUE)) {
    stop("Highest point on earth is 8848m above sea level")
  }
}

latitude <- function(locale) {
  # Latitude of location
  field(locale, "latitude")
}

longitude <- function(locale) {
  # Longitude of location
  field(locale, "longitude")
}

elevation <- function(locale) {
  # Elevation of location
  field(locale, "elevation")
}

zone <- function(locale) {
  # Time zone of location
  field(locale, "zone")
}

#' @export
vec_ptype_abbr.location <- function(x, ...) {
  "Loc"
}

#' @export
format.location <- function(x, ...) {
  sprintf(
    "(%.2f,%.2f)^%.0f[%.3f]",
    latitude(x),
    longitude(x),
    elevation(x),
    zone(x)
  )
}

#' @export
vec_ptype2.location.location <- function(x, y, ...) location()
