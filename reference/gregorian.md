# Gregorian calendar dates

The Gregorian calendar is the standard calendar used by most of the
world. It was named for Pope Gregory XIII who introduced it in 1582. It
is a modification of the Julian calendar which was in use at the time.

## Usage

``` r
gregorian_date(year = integer(), month = integer(), day = integer())

as_gregorian(date)
```

## Arguments

- year:

  A numeric vector of years

- month:

  A numeric vector of months

- day:

  A numeric vector of days

- date:

  Vector of dates on some calendar

## Value

A gregorian vector object

## See also

[cal_gregorian](https://pkg.robjhyndman.com/calcal/reference/new_calendar.md)

## Examples

``` r
new_date(year = 2025, month = 3, day = 2:4, calendar = cal_gregorian)
#> <gregorian[3]>
#> [1] 2025-Mar-02 2025-Mar-03 2025-Mar-04
gregorian_date(2025, 4, 19:30)
#> <gregorian[12]>
#>  [1] 2025-Apr-19 2025-Apr-20 2025-Apr-21 2025-Apr-22 2025-Apr-23 2025-Apr-24
#>  [7] 2025-Apr-25 2025-Apr-26 2025-Apr-27 2025-Apr-28 2025-Apr-29 2025-Apr-30
as_date(Sys.Date(), calendar = cal_gregorian)
#> <gregorian[1]>
#> [1] 2025-Dec-30
as_gregorian(Sys.Date())
#> <gregorian[1]>
#> [1] 2025-Dec-30
as_gregorian("2016-01-01")
#> <gregorian[1]>
#> [1] 2016-Jan-01
tibble::tibble(
  x = seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"),
  y = as_gregorian(x),
  z = as_date(x, calendar = cal_gregorian)
)
#> # A tibble: 365 × 3
#>    x                    y           z
#>    <date>           <Gre>       <Gre>
#>  1 2025-01-01 2025-Jan-01 2025-Jan-01
#>  2 2025-01-02 2025-Jan-02 2025-Jan-02
#>  3 2025-01-03 2025-Jan-03 2025-Jan-03
#>  4 2025-01-04 2025-Jan-04 2025-Jan-04
#>  5 2025-01-05 2025-Jan-05 2025-Jan-05
#>  6 2025-01-06 2025-Jan-06 2025-Jan-06
#>  7 2025-01-07 2025-Jan-07 2025-Jan-07
#>  8 2025-01-08 2025-Jan-08 2025-Jan-08
#>  9 2025-01-09 2025-Jan-09 2025-Jan-09
#> 10 2025-01-10 2025-Jan-10 2025-Jan-10
#> # ℹ 355 more rows
```
