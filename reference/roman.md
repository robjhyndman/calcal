# Roman calendar dates

The Roman calendar (as defined here) is the same as the Julian calendar
but with different nomenclature. Rather than use a (year, month, day)
triple for each date, it specifies dates using year, month, event,
count.

## Usage

``` r
roman_date(
  year = integer(),
  month = integer(),
  event = integer(),
  count = integer(),
  leap_day = logical()
)

as_roman(date)
```

## Arguments

- year:

  A numeric vector of years

- month:

  A numeric vector of months

- event:

  A numeric vector of events: 1 = Kalends, 2 = Nones, 3 = Ides

- count:

  A numeric vector of counts

- leap_day:

  A logical vector indicating if day is a leap day

- date:

  Vector of dates on some calendar

## Value

A roman vector object

## See also

[cal_roman](https://pkg.robjhyndman.com/calcal/reference/new_calendar.md)

## Examples

``` r
roman_date(66, 4, 1, 1, FALSE)
#> [1] <NA>
new_date(year = 66, month = 4, event = 1, count = 1, leap_day = FALSE, calendar = cal_roman)
#> [1] <NA>
as_roman("2016-01-01")
#> [1] <NA>
tibble::tibble(
  x = seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"),
  y = as_roman(x)
)
#> # A tibble: 365 × 2
#>    x                                 y
#>    <date>                        <Rom>
#>  1 2025-01-01  2025-Jan-ad_xiv_Kalends
#>  2 2025-01-02 2025-Jan-ad_xiii_Kalends
#>  3 2025-01-03  2025-Jan-ad_xii_Kalends
#>  4 2025-01-04   2025-Jan-ad_xi_Kalends
#>  5 2025-01-05    2025-Jan-ad_x_Kalends
#>  6 2025-01-06   2025-Jan-ad_ix_Kalends
#>  7 2025-01-07 2025-Jan-ad_viii_Kalends
#>  8 2025-01-08  2025-Jan-ad_vii_Kalends
#>  9 2025-01-09   2025-Jan-ad_vi_Kalends
#> 10 2025-01-10    2025-Jan-ad_v_Kalends
#> # ℹ 355 more rows
```
