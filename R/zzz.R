# To process last when building the package

# Month constants for Julian/Gregorian calendar
JANUARY <- 1
FEBRUARY <- JANUARY + 1
MARCH <- JANUARY + 2
APRIL <- JANUARY + 3
MAY <- JANUARY + 4
JUNE <- JANUARY + 5
JULY <- JANUARY + 6
AUGUST <- JANUARY + 7
SEPTEMBER <- JANUARY + 8
OCTOBER <- JANUARY + 9
NOVEMBER <- JANUARY + 10
DECEMBER <- JANUARY + 11

# Day of week constants
SUNDAY <- 0
MONDAY <- SUNDAY + 1
TUESDAY <- SUNDAY + 2
WEDNESDAY <- SUNDAY + 3
THURSDAY <- SUNDAY + 4
FRIDAY <- SUNDAY + 5
SATURDAY <- SUNDAY + 6

GREGORIAN_EPOCH <- 1 # Fixed date of start of the (proleptic) Gregorian calendar
JULIAN_EPOCH <- vec_data(as_rd(gregorian(0, DECEMBER, 30)))

MECCA <- location(angle(21, 25, 24), angle(39, 49, 24), 1000, 2)  # Location of Mecca
HAIFA <- location(32.82, 35, 0, 2)  # Location of Haifa, Israel
JERUSALEM <- location(31.8, 35.2, 800, 2)  # Location of Jerusalem

#J2000 <- hr(12) + as_rd(gregorian(2000, JANUARY, 1))  # Noon at start of Gregorian year 2000
J2000 <- rd_fixed(730120.5)

MEAN_TROPICAL_YEAR <- 365.242189
MEAN_SYNODIC_MONTH <- 29.530588853

# Seasons
SPRING <- deg(0)    # Longitude of sun at vernal equinox
SUMMER <- deg(90)   # Longitude of sun at summer solstice
AUTUMN <- deg(180)  # Longitude of sun at autumnal equinox
WINTER <- deg(270)  # Longitude of sun at winter solstice

# Moon phases
NEW <- deg(0)             # New moon
FIRST_QUARTER <- deg(90)  # First quarter moon
FULL <- deg(180)          # Full moon
LAST_QUARTER <- deg(270)  # Last quarter moon
ISLAMIC_LOCALE <- location(deg(30.1), deg(31.3), mt(200), 2)  # Sample location for Observational Islamic calendar (Cairo, Egypt)

