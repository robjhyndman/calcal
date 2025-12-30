# Persian dates

The modern Persian calendar was adopted in 1925 in Iran and in 1957 in
Afghanistan. An alternative version of the calendar, using only
arithmetic (rather than astronomical) calculations is available as the
`apersian` calendar.

## Usage

``` r
persian_date(year = integer(), month = integer(), day = integer())

apersian_date(year = integer(), month = integer(), day = integer())

as_persian(date)

as_apersian(date)
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

A persian vector object

## Examples

``` r
gregorian_date(2025,5,1:20) |>
  as_persian()
#> <persian[20]>
#>  [1] 1404-Ordi-11 1404-Ordi-12 1404-Ordi-13 1404-Ordi-14 1404-Ordi-15
#>  [6] 1404-Ordi-16 1404-Ordi-17 1404-Ordi-18 1404-Ordi-19 1404-Ordi-20
#> [11] 1404-Ordi-21 1404-Ordi-22 1404-Ordi-23 1404-Ordi-24 1404-Ordi-25
#> [16] 1404-Ordi-26 1404-Ordi-27 1404-Ordi-28 1404-Ordi-29 1404-Ordi-30
```
