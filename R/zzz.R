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

MECCA <- location(angle(21, 25, 24), angle(39, 49, 24), 1000, 2) # Location of Mecca
HAIFA <- location(32.82, 35, 0, 2) # Location of Haifa, Israel
JERUSALEM <- location(31.8, 35.2, 800, 2) # Location of Jerusalem

J2000 <- vec_data(as_rd("2000-01-01")) + 12 / 24 # Noon at start of Gregorian year 2000

MEAN_TROPICAL_YEAR <- 365.242189
MEAN_SIDERIAL_YEAR <- 365.25636
MEAN_SYNODIC_MONTH <- 29.530588853

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
ISLAMIC_LOCALE <- location(deg(30.1), deg(31.3), mt(200), 2) # Sample location for Observational Islamic calendar (Cairo, Egypt)
