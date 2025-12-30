# Chinese holidays

Dates are returned as Gregorian dates

## Usage

``` r
chinese_new_year(year)

dragon_festival(year)

qing_ming(year)
```

## Arguments

- year:

  The year on the Gregorian calendar

## Value

A vector of dates on the Gregorian calendar

## See also

[chinese_date](https://pkg.robjhyndman.com/calcal/reference/chinese.md)

## Examples

``` r
tibble::tibble(
  year = 2025:2030,
  cny = chinese_new_year(year),
  qm = qing_ming(year),
  dbf = dragon_festival(year)
)
#> # A tibble: 6 × 4
#>    year         cny          qm         dbf
#>   <int>       <Gre>       <Gre>       <Gre>
#> 1  2025 2025-Jan-29 2025-Apr-04 2025-May-31
#> 2  2026 2026-Feb-17 2026-Apr-05 2026-Jun-19
#> 3  2027 2027-Feb-06 2027-Apr-05 2027-Jun-09
#> 4  2028 2028-Jan-26 2028-Apr-04 2028-May-28
#> 5  2029 2029-Feb-13 2029-Apr-04 2029-Jun-16
#> 6  2030 2030-Feb-03 2030-Apr-05 2030-Jun-05
```
