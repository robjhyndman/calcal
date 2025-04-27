#' Locations
#'
#' Create a location object
#'
#' @param latitude A numeric vector of latitudes
#' @param longitude A numeric vector of longitudes
#' @param elevation A numeric vector of elevations above sea level
#' @param zone A numeric vector of time zones (relative to UTC)
#' @return A location vector object
#' @examples
#' melbourne <- location(-37.8136, 144.9631, 31, 10)
#' @export
location <- function(
  latitude = numeric(),
  longitude = numeric(),
  elevation = numeric(),
  zone = numeric()
) {
  lst <- vec_cast_common(latitude = latitude, longitude = longitude, elevation = elevation, zone = zone, .to = numeric())
  lst <- vec_recycle_common(latitude = lst$latitude, longitude = lst$longitude, elevation = lst$elevation, zone = lst$zone)
  check_locale(lst)
  new_rcrd(lst, class = "location")
}

check_locale <- function(args) {
  latitude <- args$latitude
  longitude <- args$longitude
  elevation <- args$elevation
  zone <- args$zone
  if(any(zone < -12 | zone > 14, na.rm = TRUE)) {
    stop("zone must be between -12 and 14")
  }
  if(any(latitude < -90 | latitude > 90, na.rm = TRUE)) {
    stop("latitude must be between -90 and 90")
  }
  if(any(longitude < -180 | longitude > 180, na.rm = TRUE)) {
    stop("longitude must be between -180 and 180")
  }
  if(any(elevation < -420, na.rm = TRUE)) {
    stop("Lowest point on earth is 420m below sea level")
  }
  if(any(elevation > 8848, na.rm = TRUE)) {
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
  sprintf("(%.2f,%.2f)^%d[%s]", latitude(x), longitude(x), elevation(x), zone(x))
}

#' @export
vec_ptype2.location.location <- function(x, y, ...) location()


direction <- function(locale, focus) {
  # Angle (clockwise from North) to face focus when standing in locale
  # Subject to errors near focus and its antipode
  phi <- latitude(locale)
  phi_prime <- latitude(focus)
  psi <- longitude(locale)
  psi_prime <- longitude(focus)

  denom <- cosine_degrees(phi) * tangent_degrees(phi_prime) -
    sin_degrees(phi) * cosine_degrees(psi - psi_prime)
  result <- arctan_degrees(
    sin_degrees(psi_prime - psi) / denom, 1 + denom < 0
  )
  (result %% 360) * (denom != 0)
}
