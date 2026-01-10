# Hebrew calendar dates

The Hebrew (or Jewish) calendar is an official calendar of Israel, and
is used for Jewish religious holidays. It is a lunisolar calendar
comprising months of 29 or 30 days, which begin and end at approximately
the time of the new moon. An extra lunar month is added every 2 or 3
years, so the calendar has either 12 or 13 months per year.

## Usage

``` r
hebrew_date(year = integer(), month = integer(), day = integer())

as_hebrew(date)

ohebrew_date(year = integer(), month = integer(), day = integer())

as_ohebrew(date)

samaritan_date(year = integer(), month = integer(), day = integer())

as_samaritan(date)
```

## Arguments

- year:

  A numeric vector of years

- month:

  A numeric vector of months

- day:

  A numeric vector of days

- date:

  Vector of dates on some calendar

## Value

A hebrew vector object

## Details

The observational Hebrew calendar ("ohebrew") is the classical calendar
where the new month began with the reported observation of the crescent
new moon. In this implementation, Haifa is taken as the point of
observation.

The Samaritan calendar is similar, but the moment of new moon marking
the start of each new month is based on a traditional reckoning of the
lunar cycle,

## See also

[cal_hebrew](https://pkg.robjhyndman.com/calcal/reference/new_calendar.md),
[rosh_hashanah](https://pkg.robjhyndman.com/calcal/reference/jewish.md)

## Examples

``` r
heb <- new_date(year = 5785, month = 3, day = 2:4, calendar = cal_hebrew)
heb
#> <hebrew[3]>
#> [1] 5785-Sivan-02 5785-Sivan-03 5785-Sivan-04
hebrew_date(5785, 3, 2:4)
#> <hebrew[3]>
#> [1] 5785-Sivan-02 5785-Sivan-03 5785-Sivan-04
as_date(heb, calendar = cal_gregorian)
#> <gregorian[3]>
#> [1] 2025-May-29 2025-May-30 2025-May-31
as_date(Sys.Date(), calendar = cal_hebrew)
#> <hebrew[1]>
#> [1] 5786-Tevet-21
tibble::tibble(
  gregorian = gregorian_date(2025, 1, 1) + 0:364,
  hebrew = as_date(gregorian, calendar = cal_hebrew),
)
#> # A tibble: 365 × 2
#>      gregorian        hebrew
#>          <Gre>         <Heb>
#>  1 2025-Jan-01 5785-Tevet-01
#>  2 2025-Jan-02 5785-Tevet-02
#>  3 2025-Jan-03 5785-Tevet-03
#>  4 2025-Jan-04 5785-Tevet-04
#>  5 2025-Jan-05 5785-Tevet-05
#>  6 2025-Jan-06 5785-Tevet-06
#>  7 2025-Jan-07 5785-Tevet-07
#>  8 2025-Jan-08 5785-Tevet-08
#>  9 2025-Jan-09 5785-Tevet-09
#> 10 2025-Jan-10 5785-Tevet-10
#> # ℹ 355 more rows
as_gregorian(hebrew_date(5785, 3, 2:4))
#> <gregorian[3]>
#> [1] 2025-May-29 2025-May-30 2025-May-31
as_hebrew(gregorian_date(2025, 1, 1:31))
#> <hebrew[31]>
#>  [1] 5785-Tevet-01  5785-Tevet-02  5785-Tevet-03  5785-Tevet-04  5785-Tevet-05 
#>  [6] 5785-Tevet-06  5785-Tevet-07  5785-Tevet-08  5785-Tevet-09  5785-Tevet-10 
#> [11] 5785-Tevet-11  5785-Tevet-12  5785-Tevet-13  5785-Tevet-14  5785-Tevet-15 
#> [16] 5785-Tevet-16  5785-Tevet-17  5785-Tevet-18  5785-Tevet-19  5785-Tevet-20 
#> [21] 5785-Tevet-21  5785-Tevet-22  5785-Tevet-23  5785-Tevet-24  5785-Tevet-25 
#> [26] 5785-Tevet-26  5785-Tevet-27  5785-Tevet-28  5785-Tevet-29  5785-Shevat-01
#> [31] 5785-Shevat-02
as_hebrew("2016-01-01")
#> <hebrew[1]>
#> [1] 5776-Tevet-20
as_hebrew(Sys.Date())
#> <hebrew[1]>
#> [1] 5786-Tevet-21
hebrew_date(5785, 3, 1:10) |> day_of_week()
#>  [1] "Revii"   "Hamishi" "Shishi"  "Shabbat" "Rishon"  "Sheni"   "Shlishi"
#>  [8] "Revii"   "Hamishi" "Shishi" 
```
