# Compute granularities from dates

Compute days, weeks, or months from a vector of dates. These work for
Gregorian dates, and for some other calendars where it makes sense. In
particular, `day_of_week` has been implemented for many calendars that
contain the concept of a week. Similarly, `day_of_month`, `day_of_year`
and `days_remaining` will work for several calendars.

## Usage

``` r
day_of_week(date, ...)

day_of_month(date)

day_of_year(date)

days_remaining(date)

week_of_month(date, first_day = "Monday")

week_of_year(date, first_day = "Monday")

month_of_year(date)

year(date)
```

## Arguments

- date:

  A vector of dates

- ...:

  Other arguments used for specific calendars

- first_day:

  Character denoting first day of the week. Default is `"Monday"`

## Value

A vector of numerical values for the requested granularity. In the case
of `day_of_week()`, it returns a character vector of the name of the day
of the week, or a numeric vector if `numeric = TRUE` is specified.

## Details

`week_of_year()` returns the ISO 8601 week number with `first_day` as
Monday. Under this standard, week 1 of a year is defined as the first
week with at least 4 days in the year; equivalently, it is the week
containing 4 January. There is no week 0; instead week 1 of a year may
begin in the previous calendar year.

`week_of_month()` is defined analogously where week 1 of a month is the
first week with at least 4 days in the month; equivalently, it is the
week containing the 4th day of the month. There is no week 0; instead
week 1 of a month may begin in the previous calendar month.

`days_remaining()` returns the number of days remaining in the year.

Other functions should be self-explanatory.

## Examples

``` r
april2025 <- gregorian_date(2025, 4, 1:30)
day_of_week(april2025)
#>  [1] "Tuesday"   "Wednesday" "Thursday"  "Friday"    "Saturday"  "Sunday"   
#>  [7] "Monday"    "Tuesday"   "Wednesday" "Thursday"  "Friday"    "Saturday" 
#> [13] "Sunday"    "Monday"    "Tuesday"   "Wednesday" "Thursday"  "Friday"   
#> [19] "Saturday"  "Sunday"    "Monday"    "Tuesday"   "Wednesday" "Thursday" 
#> [25] "Friday"    "Saturday"  "Sunday"    "Monday"    "Tuesday"   "Wednesday"
day_of_month(april2025)
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
#> [26] 26 27 28 29 30
day_of_year(april2025)
#>  [1]  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109
#> [20] 110 111 112 113 114 115 116 117 118 119 120
days_remaining(april2025)
#>  [1] 274 273 272 271 270 269 268 267 266 265 264 263 262 261 260 259 258 257 256
#> [20] 255 254 253 252 251 250 249 248 247 246 245
week_of_month(april2025)
#>  [1] 1 1 1 1 1 1 2 2 2 2 2 2 2 3 3 3 3 3 3 3 4 4 4 4 4 4 4 1 1 1
week_of_year(april2025)
#>  [1] 14 14 14 14 14 14 15 15 15 15 15 15 15 16 16 16 16 16 16 16 17 17 17 17 17
#> [26] 17 17 18 18 18
month_of_year(april2025)
#>  [1] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
```
