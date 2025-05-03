#==============================================================================
# Akan Calendar
#==============================================================================

akan_name <- function(prefix, stem) {
  c(prefix, stem)
}

akan_prefix <- function(name) {
  name[1]
}

akan_stem <- function(name) {
  name[2]
}

akan_day_name <- function(n) {
  akan_name(amod(n, 6), amod(n, 7))
}

akan_name_difference <- function(a_name1, a_name2) {
  prefix1 <- akan_prefix(a_name1)
  prefix2 <- akan_prefix(a_name2)
  stem1 <- akan_stem(a_name1)
  stem2 <- akan_stem(a_name2)
  prefix_difference <- prefix2 - prefix1
  stem_difference <- stem2 - stem1
  
  amod(prefix_difference + 36 * (stem_difference - prefix_difference), 42)
}

AKAN_DAY_NAME_EPOCH <- rd(37)

akan_name_from_fixed <- function(date) {
  akan_day_name(date - AKAN_DAY_NAME_EPOCH)
}

akan_day_name_on_or_before <- function(name, date) {
  mod3(akan_name_difference(akan_name_from_fixed(0), name), date, date - 42)
}
