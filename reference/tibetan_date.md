# Tibetan calendar dates

There are several Tibetan calendars. These functions implement the
official Phuglugs version of the Kalachakra calendar, which is similar
to the Hindu lunisolar calendars.

## Usage

``` r
tibetan_date(
  year = integer(),
  month = integer(),
  leap_month = logical(),
  day = integer(),
  leap_day = logical()
)

as_tibetan(date)
```

## Arguments

- year:

  A numeric vector of years

- month:

  A numeric vector of months

- leap_month:

  A logical vector of leap months

- day:

  A numeric vector of days

- leap_day:

  A logical vector of leap days

- date:

  A vector of dates on some calendar

## Value

A `tibetan_date` object

## See also

[tibetan_new_year](https://pkg.robjhyndman.com/calcal/reference/tibetan_new_year.md)

## Examples

``` r
gregorian_date(2025,6,1:10) |> as_tibetan()
#> <tibetan[10]>
#>  [1] 2152-snron-6   2152-snron-7   2152-snron-8   2152-snron-9   2152-snron-10 
#>  [6] 2152-snron-11  2152-snron-12  2152-snron-12* 2152-snron-13  2152-snron-14 
```
