# Bahá'í calendar dates

The Bahá'í calendar is a solar calendar used in the Bahá'í faith
comprising 18 months, with four or five intercalary days each year. The
New Year is at the northern Spring equinox, corresponding to 21 March on
the Gregorian calendar. Ayyám-i-Há is specified as month 20.

## Usage

``` r
bahai_date(
  major = integer(),
  cycle = integer(),
  year = integer(),
  month = integer(),
  day = integer()
)

as_bahai(date)
```

## Arguments

- major:

  A numeric vector of major periods

- cycle:

  A numeric vector of cycles

- year:

  A numeric vector of years within the cycles

- month:

  A numeric vector of months

- day:

  A numeric vector of days

- date:

  A numeric vector of dates

## Value

A bahai vector object

## See also

[cal_bahai](https://pkg.robjhyndman.com/calcal/reference/new_calendar.md),
[bahai_new_year](https://pkg.robjhyndman.com/calcal/reference/bahai_new_year.md)

## Examples

``` r
tibble::tibble(
  gregorian = gregorian_date(2025, 2, 15) + 0:30,
  bahai = as_bahai(gregorian)
)
#> # A tibble: 31 × 2
#>      gregorian            bahai
#>          <Gre>            <Bah>
#>  1 2025-Feb-15 01-10-10-Mulk-09
#>  2 2025-Feb-16 01-10-10-Mulk-10
#>  3 2025-Feb-17 01-10-10-Mulk-11
#>  4 2025-Feb-18 01-10-10-Mulk-12
#>  5 2025-Feb-19 01-10-10-Mulk-13
#>  6 2025-Feb-20 01-10-10-Mulk-14
#>  7 2025-Feb-21 01-10-10-Mulk-15
#>  8 2025-Feb-22 01-10-10-Mulk-16
#>  9 2025-Feb-23 01-10-10-Mulk-17
#> 10 2025-Feb-24 01-10-10-Mulk-18
#> # ℹ 21 more rows
bahai_date(1, 10, 11, 3, 5:7)
#> <bahai[3]>
#> [1] 01-10-11-Jamal-05 01-10-11-Jamal-06 01-10-11-Jamal-07
```
