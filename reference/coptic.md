# Coptic and Ethoiopic calendar dates

These two calendars are identical apart from the starting point or
epoch. The Coptic calendar (also called the Alexandrian calendar) starts
on 29 August 284 CE in the Julian calendar, while the Ethiopic (or
Ethiopian) calendar starts on 29 August 8 CE in the Julian calendar. The
Coptic calendar is used by the Coptic Orthodox and Coptic Catholic
Churches, while the Ethiopic calendar is the official state calendar of
Ethiopia, and unofficial calendar of Eritrea, and is used by the
Ethiopian and Eritrean Orthodox Churches. Both calendars have 13 months,
with 12 months of 30 days and a 13th month of 5 or 6 days depending on
whether it is a leap year. Leap years occur every 4 years.

## Usage

``` r
coptic_date(year = integer(), month = integer(), day = integer())

ethiopic_date(year = integer(), month = integer(), day = integer())

as_coptic(date)

as_ethiopic(date)
```

## Arguments

- year:

  A numeric vector of years

- month:

  A numeric vector of months

- day:

  A numeric vector of days

- date:

  A numeric vector of dates

## Value

A coptic or ethiopic vector object

## See also

[cal_coptic](https://pkg.robjhyndman.com/calcal/reference/new_calendar.md),
[cal_ethiopic](https://pkg.robjhyndman.com/calcal/reference/new_calendar.md),
[coptic_christmas](https://pkg.robjhyndman.com/calcal/reference/christian.md)

## Examples

``` r
tibble::tibble(
  gregorian = gregorian_date(2025, 1, 1:31),
  coptic = as_coptic(gregorian),
  ethiopic = as_ethiopic(gregorian)
)
#> # A tibble: 31 × 3
#>      gregorian      coptic     ethiopic
#>          <Gre>       <Cop>        <Eth>
#>  1 2025-Jan-01 1741-Koi-23 2017-Takh-23
#>  2 2025-Jan-02 1741-Koi-24 2017-Takh-24
#>  3 2025-Jan-03 1741-Koi-25 2017-Takh-25
#>  4 2025-Jan-04 1741-Koi-26 2017-Takh-26
#>  5 2025-Jan-05 1741-Koi-27 2017-Takh-27
#>  6 2025-Jan-06 1741-Koi-28 2017-Takh-28
#>  7 2025-Jan-07 1741-Koi-29 2017-Takh-29
#>  8 2025-Jan-08 1741-Koi-30 2017-Takh-30
#>  9 2025-Jan-09 1741-Tob-01  2017-Ter-01
#> 10 2025-Jan-10 1741-Tob-02  2017-Ter-02
#> # ℹ 21 more rows
coptic_date(1741, 5, 16:18)
#> <coptic[3]>
#> [1] 1741-Tob-16 1741-Tob-17 1741-Tob-18
as_date(Sys.Date(), calendar = cal_ethiopic)
#> <ethiopic[1]>
#> [1] 2018-Takh-21
as_coptic("2016-01-01")
#> <coptic[1]>
#> [1] 1732-Koi-22
as_ethiopic(Sys.Date())
#> <ethiopic[1]>
#> [1] 2018-Takh-21
```
