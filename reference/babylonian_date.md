# Babylonian calendar dates

The classical Babylonian calendar was a lunisolar calendar with a fixed
19-year Metonic cycle.

## Usage

``` r
babylonian_date(
  year = integer(),
  month = integer(),
  leap_month = logical(),
  day = integer()
)

as_babylonian(date)
```

## Arguments

- year:

  Numeric vector of years

- month:

  Numeric vector of months

- leap_month:

  Logical vector of leap months

- day:

  Numeric vector of days

- date:

  Vector of dates on some calendar.

## Value

A babylonian vector object

## See also

[cal_babylonian](https://pkg.robjhyndman.com/calcal/reference/new_calendar.md)

## Examples

``` r
tibble::tibble(
  gregorian = gregorian_date(2335, 1, 1:2),
  babylonian = as_babylonian(gregorian)
)
#> # A tibble: 2 × 2
#>     gregorian   babylonian
#>         <Gre>        <Bab>
#> 1 2335-Jan-01 2645-Tebe-04
#> 2 2335-Jan-02 2645-Tebe-05
babylonian_date(2335, 6, FALSE, 1:2)
#> <babylonian[2]>
#> [1] 2335-Ulul-01 2335-Ulul-02
```
