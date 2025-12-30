# ISO calendar dates

In ISO 8601 date objects, weeks are defined as starting on Mondays. Week
1 is the first week with at least 4 days in the year. Equivalently, it
is the week containing 4 January. There is no week 0; instead week 1 of
a year may begin in the previous calendar year.

## Usage

``` r
iso_date(year = integer(), week = integer(), day = integer())

as_iso(date)
```

## Arguments

- year:

  A numeric vector of years

- week:

  A numeric vector of weeks

- day:

  A numeric vector of days

- date:

  Vector of dates on some calendar

## Value

An iso vector object

## Details

More flexible week numbering is possible using Gregorian dates with
[`week_of_year()`](https://pkg.robjhyndman.com/calcal/reference/gregorian-parts.md).

## See also

[cal_iso](https://pkg.robjhyndman.com/calcal/reference/new_calendar.md),
[week_of_year](https://pkg.robjhyndman.com/calcal/reference/gregorian-parts.md)

## Examples

``` r
iso <- new_date(year = 2025, week = 23, day = 2:4, calendar = cal_iso)
iso
#> <iso[3]>
#> [1] 2025-23-02 2025-23-03 2025-23-04
iso_date(2025, 23, 2:4)
#> <iso[3]>
#> [1] 2025-23-02 2025-23-03 2025-23-04
as_gregorian(iso_date(2025, 23, 2:4))
#> <gregorian[3]>
#> [1] 2025-Jun-03 2025-Jun-04 2025-Jun-05
as_iso(gregorian_date(2025, 1, 1:31))
#> <iso[31]>
#>  [1] 2025-01-03 2025-01-04 2025-01-05 2025-01-06 2025-01-07 2025-02-01
#>  [7] 2025-02-02 2025-02-03 2025-02-04 2025-02-05 2025-02-06 2025-02-07
#> [13] 2025-03-01 2025-03-02 2025-03-03 2025-03-04 2025-03-05 2025-03-06
#> [19] 2025-03-07 2025-04-01 2025-04-02 2025-04-03 2025-04-04 2025-04-05
#> [25] 2025-04-06 2025-04-07 2025-05-01 2025-05-02 2025-05-03 2025-05-04
#> [31] 2025-05-05
as_iso("2016-01-01")
#> <iso[1]>
#> [1] 2015-53-05
as_iso(Sys.Date())
#> <iso[1]>
#> [1] 2026-01-02
tibble::tibble(
  x = seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"),
  y = as_iso(x)
)
#> # A tibble: 365 × 2
#>    x                   y
#>    <date>          <ISO>
#>  1 2025-01-01 2025-01-03
#>  2 2025-01-02 2025-01-04
#>  3 2025-01-03 2025-01-05
#>  4 2025-01-04 2025-01-06
#>  5 2025-01-05 2025-01-07
#>  6 2025-01-06 2025-02-01
#>  7 2025-01-07 2025-02-02
#>  8 2025-01-08 2025-02-03
#>  9 2025-01-09 2025-02-04
#> 10 2025-01-10 2025-02-05
#> # ℹ 355 more rows
```
