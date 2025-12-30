# US Holidays

Functions to return Gregorian dates for US holidays and other special
days

## Usage

``` r
us_memorial_day(year)

us_independence_day(year)

us_labor_day(year)

us_election_day(year)

us_daylight_saving_start(year)

us_daylight_saving_end(year)

unlucky_fridays(year)
```

## Arguments

- year:

  Gregorian year

## Value

A vector of Gregorian dates corresponding to the US holidays or special
days.

## Examples

``` r
us_memorial_day(2025)
#> <gregorian[1]>
#> [1] 2025-May-26
us_independence_day(2025)
#> <gregorian[1]>
#> [1] 2025-Jul-04
us_labor_day(2025)
#> <gregorian[1]>
#> [1] 2025-Sep-01
us_election_day(2025)
#> <gregorian[1]>
#> [1] 2025-Nov-04
us_daylight_saving_start(2025)
#> <gregorian[1]>
#> [1] 2025-Mar-09
us_daylight_saving_end(2025)
#> <gregorian[1]>
#> [1] 2025-Nov-02
unlucky_fridays(2025)
#> <gregorian[1]>
#> [1] 2025-Jun-13
```
