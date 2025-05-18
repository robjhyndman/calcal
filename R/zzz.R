# To process last when building the package

# Month constants for Julian/Gregorian calendar
JANUARY <- 1L
FEBRUARY <- JANUARY + 1L
MARCH <- JANUARY + 2L
APRIL <- JANUARY + 3L
MAY <- JANUARY + 4L
JUNE <- JANUARY + 5L
JULY <- JANUARY + 6L
AUGUST <- JANUARY + 7L
SEPTEMBER <- JANUARY + 8L
OCTOBER <- JANUARY + 9L
NOVEMBER <- JANUARY + 10L
DECEMBER <- JANUARY + 11L

# Day of week constants
SUNDAY <- 0L
MONDAY <- SUNDAY + 1L
TUESDAY <- SUNDAY + 2L
WEDNESDAY <- SUNDAY + 3L
THURSDAY <- SUNDAY + 4L
FRIDAY <- SUNDAY + 5L
SATURDAY <- SUNDAY + 6L

GREGORIAN_EPOCH <- 1L # Fixed date of start of the (proleptic) Gregorian calendar
JULIAN_EPOCH <- vec_data(as_rd(gregorian(0, DECEMBER, 30)))
JD_EPOCH <- -1721424.5
MJD_EPOCH <- 678576
UNIX_EPOCH <- 719163

# Locations
MECCA <- location(angle(21, 25, 24), angle(39, 49, 24), mt(298), 3)
JERUSALEM <- location(angle(31.78, 0, 0), angle(35.24, 0, 0), mt(740), 2)
TEHRAN <- location(angle(35.68, 0, 0), angle(51.42, 0, 0), mt(1100), 3.5)
BABYLON <- location(angle(32.4794, 0, 0), angle(44.4328, 0, 0), mt(26), 3.5)
UJJAIN <- location(angle(23, 9, 0), angle(75, 46, 6), mt(0), 5 + 461/9000)

J2000 <- vec_data(as_rd("2000-01-01")) + hr(12) # Noon at start of Gregorian year 2000

MEAN_TROPICAL_YEAR <- 365.242189
MEAN_SIDEREAL_YEAR <- 365.25636
MEAN_SYNODIC_MONTH <- 29.530588861

# Seasons
SPRING <- deg(0) # Longitude of sun at vernal equinox
SUMMER <- deg(90) # Longitude of sun at summer solstice
AUTUMN <- deg(180) # Longitude of sun at autumnal equinox
WINTER <- deg(270) # Longitude of sun at winter solstice

# Moon phases
NEW <- deg(0) # New moon
FIRST_QUARTER <- deg(90) # First quarter moon
FULL <- deg(180) # Full moon
LAST_QUARTER <- deg(270) # Last quarter moon

# Time of day
MORNING <- TRUE
EVENING <- FALSE
