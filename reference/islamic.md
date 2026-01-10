# Islamic calendar dates

The Islamic (or Hijri) calendar is a lunar calendar comprising 12 lunar
months in a year of 354 or 355 days. It is widely used in for Islamic
holidays, and in countries where the predominant religion is Islam.

## Usage

``` r
islamic_date(year = integer(), month = integer(), day = integer())

as_islamic(date)

oislamic_date(year = integer(), month = integer(), day = integer())

as_oislamic(date)

saudi_date(year = integer(), month = integer(), day = integer())

as_saudi(date)
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

An islamic vector object

## Details

Three variations are implemented here. The standard Islamic calendar is
available using `as_islamic` and `islamic_date`. The Saudi Islamic
calendar uses `as_saudi` and `saudi_date`, while the traditional
observational Islamic calendar is available using `as_oislamic` and
`oislamic_date`.

## See also

[cal_islamic](https://pkg.robjhyndman.com/calcal/reference/new_calendar.md),
[ramadan](https://pkg.robjhyndman.com/calcal/reference/islamic_holidays.md)

## Examples

``` r
islamic_date(2025, 5, 1:30)
#> <islamic[30]>
#>  [1] 2025-Jum1-01 2025-Jum1-02 2025-Jum1-03 2025-Jum1-04 2025-Jum1-05
#>  [6] 2025-Jum1-06 2025-Jum1-07 2025-Jum1-08 2025-Jum1-09 2025-Jum1-10
#> [11] 2025-Jum1-11 2025-Jum1-12 2025-Jum1-13 2025-Jum1-14 2025-Jum1-15
#> [16] 2025-Jum1-16 2025-Jum1-17 2025-Jum1-18 2025-Jum1-19 2025-Jum1-20
#> [21] 2025-Jum1-21 2025-Jum1-22 2025-Jum1-23 2025-Jum1-24 2025-Jum1-25
#> [26] 2025-Jum1-26 2025-Jum1-27 2025-Jum1-28 2025-Jum1-29 2025-Jum1-30
as_islamic("2016-01-01")
#> <islamic[1]>
#> [1] 1437-Rab1-20
as_islamic(Sys.Date())
#> <islamic[1]>
#> [1] 1447-Raj-21
tibble::tibble(
  x = seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"),
  y = as_islamic(x)
)
#> # A tibble: 365 × 2
#>    x                    y
#>    <date>           <Hij>
#>  1 2025-01-01 1446-Raj-01
#>  2 2025-01-02 1446-Raj-02
#>  3 2025-01-03 1446-Raj-03
#>  4 2025-01-04 1446-Raj-04
#>  5 2025-01-05 1446-Raj-05
#>  6 2025-01-06 1446-Raj-06
#>  7 2025-01-07 1446-Raj-07
#>  8 2025-01-08 1446-Raj-08
#>  9 2025-01-09 1446-Raj-09
#> 10 2025-01-10 1446-Raj-10
#> # ℹ 355 more rows
islamic_date(2025, 5, 1:10) |> day_of_week()
#>  [1] "al-Ithnayn"   "ath-Thulatha" "al-Arba'a"    "al-Khamis"    "al-Jumu'ah"  
#>  [6] "as-Sabt"      "al-Ahad"      "al-Ithnayn"   "ath-Thulatha" "al-Arba'a"   
islamic_date(2025, 4, 19:30)
#> <islamic[12]>
#>  [1] 2025-Rab2-19 2025-Rab2-20 2025-Rab2-21 2025-Rab2-22 2025-Rab2-23
#>  [6] 2025-Rab2-24 2025-Rab2-25 2025-Rab2-26 2025-Rab2-27 2025-Rab2-28
#> [11] 2025-Rab2-29 2025-Jum1-01
```
