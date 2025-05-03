#==============================================================================
# Ecclesiastical Calendars
#==============================================================================

orthodox_easter <- function(g_year) {
  shifted_epact <- (14 + 11 * (g_year %% 19)) %% 30
  j_year <- if (g_year > 0) g_year else g_year - 1
  paschal_moon <- fixed_from_julian(julian_date(j_year, APRIL, 19)) - shifted_epact
  
  kday_after(SUNDAY, paschal_moon)
}

alt_orthodox_easter <- function(g_year) {
  paschal_moon <- 354 * g_year +
                  30 * ((7 * g_year + 8) %/% 19) +
                  g_year %/% 4 - 
                  g_year %/% 19 - 
                  273 + 
                  GREGORIAN_EPOCH
  
  kday_after(SUNDAY, paschal_moon)
}

easter <- function(g_year) {
  century <- 1 + g_year %/% 100
  shifted_epact <- (14 + 11 * (g_year %% 19) - 
                    (3 * century) %/% 4 + 
                    (5 + 8 * century) %/% 25) %% 30
  
  adjusted_epact <- if(shifted_epact == 0 || 
                       (shifted_epact == 1 && (g_year %% 19) > 10)) {
    shifted_epact + 1
  } else {
    shifted_epact
  }
  
  paschal_moon <- fixed_from_gregorian(
    gregorian_date(g_year, APRIL, 19)
  ) - adjusted_epact
  
  kday_after(SUNDAY, paschal_moon)
}

pentecost <- function(g_year) {
  easter(g_year) + 49
}
