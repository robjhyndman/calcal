# Julian calendar dates

The Julian calendar is the calendar used by the Roman Empire, and takes
its name from Julius Caesar, who introduced it in 46 BC. It is still
used as a religious calendar in parts of the Eastern Orthodox Church.

## Usage

``` r
julian_date(year = integer(), month = integer(), day = integer())

as_julian(date)
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

A julian vector object

## See also

[cal_julian](https://pkg.robjhyndman.com/calcal/reference/new_calendar.md)

## Examples

``` r
as_date("2016-01-01", calendar = cal_julian)
#> <julian[1]>
#> [1] 2015-Dec-19
as_date(Sys.Date(), calendar = cal_julian)
#> <julian[1]>
#> [1] 2026-Feb-12
tibble::tibble(
  x = seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"),
  y = as_date(x, calendar = cal_gregorian),
  z = as_date(x, calendar = cal_julian)
)
#> # A tibble: 365 × 3
#>    x                    y           z
#>    <date>           <Gre>       <Jul>
#>  1 2025-01-01 2025-Jan-01 2024-Dec-19
#>  2 2025-01-02 2025-Jan-02 2024-Dec-20
#>  3 2025-01-03 2025-Jan-03 2024-Dec-21
#>  4 2025-01-04 2025-Jan-04 2024-Dec-22
#>  5 2025-01-05 2025-Jan-05 2024-Dec-23
#>  6 2025-01-06 2025-Jan-06 2024-Dec-24
#>  7 2025-01-07 2025-Jan-07 2024-Dec-25
#>  8 2025-01-08 2025-Jan-08 2024-Dec-26
#>  9 2025-01-09 2025-Jan-09 2024-Dec-27
#> 10 2025-01-10 2025-Jan-10 2024-Dec-28
#> # ℹ 355 more rows
new_date(year = 2025, month = 4, day = 19:30, calendar = cal_julian)
#> <julian[12]>
#>  [1] 2025-Apr-19 2025-Apr-20 2025-Apr-21 2025-Apr-22 2025-Apr-23 2025-Apr-24
#>  [7] 2025-Apr-25 2025-Apr-26 2025-Apr-27 2025-Apr-28 2025-Apr-29 2025-Apr-30
julian_date(2025, 4, 19:30)
#> <julian[12]>
#>  [1] 2025-Apr-19 2025-Apr-20 2025-Apr-21 2025-Apr-22 2025-Apr-23 2025-Apr-24
#>  [7] 2025-Apr-25 2025-Apr-26 2025-Apr-27 2025-Apr-28 2025-Apr-29 2025-Apr-30
as_julian("2016-01-01")
#> <julian[1]>
#> [1] 2015-Dec-19
as_julian(Sys.Date())
#> <julian[1]>
#> [1] 2026-Feb-12
tibble::tibble(
  x = seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"),
  y = as_julian(x)
)
#> # A tibble: 365 × 2
#>    x                    y
#>    <date>           <Jul>
#>  1 2025-01-01 2024-Dec-19
#>  2 2025-01-02 2024-Dec-20
#>  3 2025-01-03 2024-Dec-21
#>  4 2025-01-04 2024-Dec-22
#>  5 2025-01-05 2024-Dec-23
#>  6 2025-01-06 2024-Dec-24
#>  7 2025-01-07 2024-Dec-25
#>  8 2025-01-08 2024-Dec-26
#>  9 2025-01-09 2024-Dec-27
#> 10 2025-01-10 2024-Dec-28
#> # ℹ 355 more rows
```
