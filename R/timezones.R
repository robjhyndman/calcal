standard_from_universal <- function(tee_rom_u, locale) {
  # Standard time from tee_rom_u in universal time at locale
  tee_rom_u + zone(locale) / 24
}

universal_from_standard <- function(tee_rom_s, locale) {
  # Universal time from tee_rom_s in standard time at locale
  tee_rom_s - zone(locale) / 24
}

local_from_universal <- function(tee_rom_u, locale) {
  # Local time from universal tee_rom_u at locale
  tee_rom_u + longitude(locale) / 360
}

universal_from_local <- function(tee_ell, locale) {
  # Universal time from local tee_ell at locale
  tee_ell - longitude(locale) / 360
}

standard_from_local <- function(tee_ell, locale) {
  # Standard time from local tee_ell at locale
  standard_from_universal(
    universal_from_local(tee_ell, locale),
    locale
  )
}

local_from_standard <- function(tee_rom_s, locale) {
  # Local time from standard tee_rom_s at locale
  local_from_universal(
    universal_from_standard(tee_rom_s, locale),
    locale
  )
}

local_from_apparent <- function(tee, locale) {
  # Local time from sundial time tee
  tee - equation_of_time(universal_from_local(tee, locale))
}

apparent_from_local <- function(tee, locale) {
  # Sundial time at local time tee
  tee + equation_of_time(universal_from_local(tee, locale))
}


universal_from_dynamical <- function(tee) {
  # Universal moment from Dynamical time tee
  tee - ephemeris_correction(tee)
}

dynamical_from_universal <- function(tee) {
  # Dynamical time at Universal moment tee
  tee + ephemeris_correction(tee)
}

midday <- function(date, locale) {
  # Standard time on fixed date of midday at locale
  standard_from_local(
    local_from_apparent(date + hr(12), locale),
    locale
  )
}

midnight <- function(date, locale) {
  # Standard time on fixed date of true (apparent) midnight at locale
  standard_from_local(
    local_from_apparent(date, locale),
    locale
  )
}
