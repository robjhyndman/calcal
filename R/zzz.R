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
