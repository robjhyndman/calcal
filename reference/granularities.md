# Canonical granularities

`granularities()` will return a character vector of canonical
granularity names for the relevant calendar. These are the granularities
used to define dates on the calendar. `granularity()` will return a
vector of numerical values for a given canonical granularity.

## Usage

``` r
granularity_names(calendar)

granularity(date, granularity)
```

## Arguments

- calendar:

  An object defining a calendar, of "calendar" class.

- date:

  A date vector on some calendar

- granularity:

  A character string indicating the granularity to extract

## Value

A character vector of granularity names or a vector of numerical values
for the specified granularity.

## See also

[week_of_year](https://pkg.robjhyndman.com/calcal/reference/gregorian-parts.md)
for some non-canonical granularities.

## Examples

``` r
granularity_names(cal_iso)
#> [1] "year" "week" "day" 
granularity_names(cal_gregorian)
#> [1] "year"  "month" "day"  
date_iso <- new_date(year = 2025, week = 23, day = 2, calendar = cal_iso)
granularity(date_iso, "week")
#> [1] 23
date_gregorian <- new_date(year = 2025, month = 1, day = 1, calendar = cal_gregorian)
granularity(date_gregorian, "month")
#> [1] 1
```
