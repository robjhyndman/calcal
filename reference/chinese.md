# Chinese, Japanese, Korean and Vietnamese calendar dates

The traditional Chinese lunisolar calendar uses a 60-year cycle with 12
months per year. The Japanese, Korean and Vietnamese calendars are
almost identical, but with different locations for determining
astronomical positions.

## Usage

``` r
chinese_date(
  cycle = integer(),
  year = integer(),
  month = integer(),
  leap_month = logical(),
  day = integer()
)

japanese_date(
  cycle = integer(),
  year = integer(),
  month = integer(),
  leap_month = logical(),
  day = integer()
)

korean_date(
  year = integer(),
  month = integer(),
  leap_month = logical(),
  day = integer()
)

vietnamese_date(
  cycle = integer(),
  year = integer(),
  month = integer(),
  leap_month = logical(),
  day = integer()
)

as_chinese(date)

as_japanese(date)

as_korean(date)

as_vietnamese(date)
```

## Arguments

- cycle:

  A numeric vector of cycles

- year:

  A numeric vector of years within the cycles

- month:

  A numeric vector of months

- leap_month:

  A logical vector indicating leap months

- day:

  A numeric vector of days

- date:

  A numeric vector of dates

## Value

A chinese vector object

## See also

[cal_chinese](https://pkg.robjhyndman.com/calcal/reference/new_calendar.md),
[chinese_new_year](https://pkg.robjhyndman.com/calcal/reference/chinese_new_year.md)

## Examples

``` r
chinese <- new_date(
  cycle = 78, year = 42, month = 5, leap_month = FALSE, day = 16:18,
  calendar = cal_chinese
)
chinese
#> <chinese[3]>
#> [1] 78-42-05-16 78-42-05-17 78-42-05-18
chinese_date(78, 42, 5, FALSE, 16:18)
#> <chinese[3]>
#> [1] 78-42-05-16 78-42-05-17 78-42-05-18
as_date(chinese, calendar = cal_gregorian)
#> <gregorian[3]>
#> [1] 2025-Jun-11 2025-Jun-12 2025-Jun-13
as_date(Sys.Date(), calendar = cal_chinese)
#> <chinese[1]>
#> [1] 78-42-11-22
tibble::tibble(
  gregorian = gregorian_date(2025, 1, 1) + 0:364,
  chinese = as_chinese(gregorian)
)
#> # A tibble: 365 × 2
#>      gregorian     chinese
#>          <Gre>       <Chi>
#>  1 2025-Jan-01 78-41-12-02
#>  2 2025-Jan-02 78-41-12-03
#>  3 2025-Jan-03 78-41-12-04
#>  4 2025-Jan-04 78-41-12-05
#>  5 2025-Jan-05 78-41-12-06
#>  6 2025-Jan-06 78-41-12-07
#>  7 2025-Jan-07 78-41-12-08
#>  8 2025-Jan-08 78-41-12-09
#>  9 2025-Jan-09 78-41-12-10
#> 10 2025-Jan-10 78-41-12-11
#> # ℹ 355 more rows
as_gregorian(chinese_date(78, 41, 12, FALSE, 3:30))
#> <gregorian[28]>
#>  [1] 2025-Jan-02 2025-Jan-03 2025-Jan-04 2025-Jan-05 2025-Jan-06 2025-Jan-07
#>  [7] 2025-Jan-08 2025-Jan-09 2025-Jan-10 2025-Jan-11 2025-Jan-12 2025-Jan-13
#> [13] 2025-Jan-14 2025-Jan-15 2025-Jan-16 2025-Jan-17 2025-Jan-18 2025-Jan-19
#> [19] 2025-Jan-20 2025-Jan-21 2025-Jan-22 2025-Jan-23 2025-Jan-24 2025-Jan-25
#> [25] 2025-Jan-26 2025-Jan-27 2025-Jan-28 2025-Jan-29
as_chinese(gregorian_date(2025, 1, 1:28))
#> <chinese[28]>
#>  [1] 78-41-12-02 78-41-12-03 78-41-12-04 78-41-12-05 78-41-12-06 78-41-12-07
#>  [7] 78-41-12-08 78-41-12-09 78-41-12-10 78-41-12-11 78-41-12-12 78-41-12-13
#> [13] 78-41-12-14 78-41-12-15 78-41-12-16 78-41-12-17 78-41-12-18 78-41-12-19
#> [19] 78-41-12-20 78-41-12-21 78-41-12-22 78-41-12-23 78-41-12-24 78-41-12-25
#> [25] 78-41-12-26 78-41-12-27 78-41-12-28 78-41-12-29
as_chinese("2016-01-01")
#> <chinese[1]>
#> [1] 78-32-11-22
as_chinese(Sys.Date())
#> <chinese[1]>
#> [1] 78-42-11-22
```
