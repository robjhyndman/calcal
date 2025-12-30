# Egyptian and Armenian calendar dates

The ancient Egyptian calendar is a 365-day solar calendar with 12 months
of 30 days each, plus a 13th month of 5 days. The Armenian calendar is
similar but has a different epoch and month names.

## Usage

``` r
egyptian_date(year = integer(), month = integer(), day = integer())

armenian_date(year = integer(), month = integer(), day = integer())

as_egyptian(date)

as_armenian(date)
```

## Arguments

- year:

  Numeric vector of years

- month:

  Numeric vector of months

- day:

  Numeric vector of days

- date:

  Vector of dates on some calendar

## Value

An egyptian or armenian vector object

## Examples

``` r
tibble::tibble(
  gregorian = gregorian_date(2025, 5, 1:10),
  egyptian = as_egyptian(gregorian),
  armenian = as_armenian(gregorian)
)
#> # A tibble: 10 × 3
#>      gregorian     egyptian     armenian
#>          <Gre>        <Egy>        <Arm>
#>  1 2025-May-01 2774-Thot-15 1474-Mare-15
#>  2 2025-May-02 2774-Thot-16 1474-Mare-16
#>  3 2025-May-03 2774-Thot-17 1474-Mare-17
#>  4 2025-May-04 2774-Thot-18 1474-Mare-18
#>  5 2025-May-05 2774-Thot-19 1474-Mare-19
#>  6 2025-May-06 2774-Thot-20 1474-Mare-20
#>  7 2025-May-07 2774-Thot-21 1474-Mare-21
#>  8 2025-May-08 2774-Thot-22 1474-Mare-22
#>  9 2025-May-09 2774-Thot-23 1474-Mare-23
#> 10 2025-May-10 2774-Thot-24 1474-Mare-24
```
