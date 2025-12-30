# Hindu solar and lunar calendar dates

There are four Hindu calendars implemented: modern Hindu solar and lunar
calendars, and the old Hindu solar and lunar calendars. Hindu solar
months are 1/12 of a solar year (approximately 30.44 days), while lunar
months are based on the lunar cycle (approximately 29.53 days).

## Usage

``` r
hindu_solar_date(year, month, day)

hindu_lunar_date(year, month, leap_month, day, leap_day)

as_hindu_solar(date)

as_hindu_lunar(date)

old_hindu_solar_date(year = integer(), month = integer(), day = integer())

old_hindu_lunar_date(
  year = integer(),
  month = integer(),
  leap_month = logical(),
  day = integer()
)

as_old_hindu_solar(date)

as_old_hindu_lunar(date)
```

## Arguments

- year:

  A numeric vector of years

- month:

  A numeric vector of months

- day:

  A numeric vector of days

- leap_month:

  A logical vector indicating if year is a leap year

- leap_day:

  A logical vector indicating if day is a leap day

- date:

  A date vector on some calendar

## Value

A vector object of Hindu dates.

## See also

[cal_hindu_solar](https://pkg.robjhyndman.com/calcal/reference/new_calendar.md),
[cal_hindu_lunar](https://pkg.robjhyndman.com/calcal/reference/new_calendar.md),
[cal_old_hindu_solar](https://pkg.robjhyndman.com/calcal/reference/new_calendar.md),
[cal_old_hindu_lunar](https://pkg.robjhyndman.com/calcal/reference/new_calendar.md),
[diwali](https://pkg.robjhyndman.com/calcal/reference/diwali.md)

## Examples

``` r
gregorian_date(2025, 1, 1:31) |>
  as_hindu_solar()
#> <hindu_solar[31]>
#>  [1] 1946-Dhan-18 1946-Dhan-19 1946-Dhan-20 1946-Dhan-21 1946-Dhan-22
#>  [6] 1946-Dhan-23 1946-Dhan-24 1946-Dhan-25 1946-Dhan-26 1946-Dhan-27
#> [11] 1946-Dhan-28 1946-Dhan-29 1946-Dhan-30 1946-Maka-01 1946-Maka-02
#> [16] 1946-Maka-03 1946-Maka-04 1946-Maka-05 1946-Maka-06 1946-Maka-07
#> [21] 1946-Maka-08 1946-Maka-09 1946-Maka-10 1946-Maka-11 1946-Maka-12
#> [26] 1946-Maka-13 1946-Maka-14 1946-Maka-15 1946-Maka-16 1946-Maka-17
#> [31] 1946-Maka-18
gregorian_date(2025, 1, 1:31) |>
  as_hindu_lunar()
#> <hindu_lunar[31]>
#>  [1] 2081-Paus-2   2081-Paus-3   2081-Paus-4   2081-Paus-5   2081-Paus-6  
#>  [6] 2081-Paus-7   2081-Paus-8   2081-Paus-9   2081-Paus-10  2081-Paus-11 
#> [11] 2081-Paus-12  2081-Paus-14  2081-Paus-15  2081-Paus-16  2081-Paus-17 
#> [16] 2081-Paus-18  2081-Paus-19  2081-Paus-20  2081-Paus-21  2081-Paus-21*
#> [21] 2081-Paus-22  2081-Paus-23  2081-Paus-24  2081-Paus-25  2081-Paus-26 
#> [26] 2081-Paus-27  2081-Paus-28  2081-Paus-29  2081-Paus-30  2081-Magh-1  
#> [31] 2081-Magh-2  
```
