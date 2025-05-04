
# Time conversion functions
standard_from_universal <- function(tee_rom_u, loc) {
  tee_rom_u + zone(loc)
}

universal_from_standard <- function(tee_rom_s, loc) {
  tee_rom_s - zone(loc)
}

zone_from_longitude <- function(phi) {
  phi / 360
}

local_from_universal <- function(tee_rom_u, loc) {
  tee_rom_u + zone_from_longitude(longitude(loc))
}

universal_from_local <- function(tee_ell, loc) {
  tee_ell - zone_from_longitude(longitude(loc))
}

standard_from_local <- function(tee_ell, loc) {
  standard_from_universal(universal_from_local(tee_ell, loc), loc)
}

local_from_standard <- function(tee_rom_s, loc) {
  local_from_universal(universal_from_standard(tee_rom_s, loc), loc)
}

apparent_from_local <- function(tee_ell, loc) {
  tee_ell + equation_of_time(universal_from_local(tee_ell, loc))
}

local_from_apparent <- function(tee, loc) {
  tee - equation_of_time(universal_from_local(tee, loc))
}

apparent_from_universal <- function(tee_rom_u, loc) {
  apparent_from_local(local_from_universal(tee_rom_u, loc), loc)
}

universal_from_apparent <- function(tee, loc) {
  universal_from_local(local_from_apparent(tee, loc), loc)
}

midnight <- function(date, loc) {
  universal_from_apparent(date, loc)
}

midday <- function(date, loc) {
  universal_from_apparent(date + hr(12), loc)
}
