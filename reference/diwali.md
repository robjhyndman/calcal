# Hindu holidays and special days

Functions to return Gregorian dates for various Hindu holidays based on
the Hindu calendars. Hindu Lunar New Year is the first day of the lunar
month of Caitra (month 1). The Birthday of Rama is on the 8th or 9th day
of the first month (Caitra). Diwali is on the new moon day in the month
of Kartik (month 8). The Great Night of Shiva is at the end of the 11
month of Magha. Mesha Sankranti is the day when the sun enters the sign
of Aries (Mesha). Sacred Wednesdays are the 8th day of the lunar month
that falls on a Wednesday. Dates may vary by a day or two due to
variations of the lunar calendar and local traditions.

## Usage

``` r
hindu_lunar_new_year(year)

mesha_sankranti(year)

diwali(year)

shiva(year)

rama(year)

sacred_wednesdays(year)
```

## Arguments

- year:

  A numeric vector of Gregorian years

## Value

A vector of dates on the Gregorian calendar

## See also

[hindu_lunar_date](https://pkg.robjhyndman.com/calcal/reference/hindu_solar_date.md)

## Examples

``` r
shiva(2025:2026)
#> <gregorian[2]>
#> [1] 2025-Feb-27 2026-Feb-16
hindu_lunar_new_year(2025:2026)
#> <gregorian[2]>
#> [1] 2025-Mar-30 2026-Mar-19
rama(2025:2026)
#> <gregorian[2]>
#> [1] 2025-Apr-06 2026-Mar-27
mesha_sankranti(2025:2026)
#> <gregorian[2]>
#> [1] 2025-Apr-14 2026-Apr-14
diwali(2025:2026)
#> <gregorian[2]>
#> [1] 2025-Oct-22 2026-Nov-10
sacred_wednesdays(2025:2026)
#> <gregorian[3]>
#> [1] 2025-Feb-05 2025-Oct-29 2026-Jul-22
```
