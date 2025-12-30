# Christian Ecclesiastical Holidays

Functions to return Gregorian dates for various Christian ecclesiastical
holidays and other special days

Coptic Christmas is celebrated on 29th of Koiak in the Coptic calendar,
which currently corresponds to 7 or 8 January in the Gregorian calendar.

## Usage

``` r
advent(year)

christmas(year)

orthodox_christmas(year)

epiphany(year)

easter(year)

orthodox_easter(year)

pentecost(year)

coptic_christmas(year)

astronomical_easter(year)
```

## Arguments

- year:

  Gregorian year

## Value

A vector of dates on the Gregorian calendar

## Examples

``` r
tibble::tibble(
  year = 2025:2030,
  advent = advent(year),
  christmas = christmas(year),
  orthodox_christmas = orthodox_christmas(year),
  epiphany = epiphany(year),
  easter = easter(year),
  orthodox_easter = orthodox_easter(year),
  pentecost = pentecost(year)
)
#> # A tibble: 6 × 8
#>    year      advent   christmas orthodox_christmas    epiphany      easter
#>   <int>       <Gre>       <Gre>              <Gre>       <Gre>       <Gre>
#> 1  2025 2025-Nov-30 2025-Dec-25        2025-Jan-07 2025-Jan-05 2025-Apr-20
#> 2  2026 2026-Nov-29 2026-Dec-25        2026-Jan-07    NA-NA-NA 2026-Apr-05
#> 3  2027 2027-Nov-28 2027-Dec-25        2027-Jan-07    NA-NA-NA 2027-Mar-28
#> 4  2028 2028-Dec-03 2028-Dec-25        2028-Jan-07    NA-NA-NA 2028-Apr-16
#> 5  2029 2029-Dec-02 2029-Dec-25        2029-Jan-07    NA-NA-NA 2029-Apr-01
#> 6  2030 2030-Dec-01 2030-Dec-25        2030-Jan-07    NA-NA-NA 2030-Apr-21
#> # ℹ 2 more variables: orthodox_easter <Gre>, pentecost <Gre>
```
