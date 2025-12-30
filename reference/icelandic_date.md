# Icelandic calendar dates

The Icelandic calendar, still in use in Iceland, divides times into
7-day weeks and two seasons: Summer and Winter. Summer starts on the
first Thursday after April 18th, and Winter 180 days earlier. Ordinary
years have 52 weeks with leap years having 53 weeks. The leap week
occurs every 5-7 years in midsummer.

## Usage

``` r
icelandic_date(
  year = integer(),
  season = integer(),
  week = integer(),
  weekday = integer()
)

as_icelandic(date)
```

## Arguments

- year:

  A numeric vector of years

- season:

  A numeric vector of seasons (1 = Summer, 2 = Winter)

- week:

  A numeric vector of weeks within the season (1 to 28)

- weekday:

  A number vector containing day of week (0 = Sunday, 1 = Monday, ..., 6
  = Saturday))

- date:

  A numeric vector of dates

## Value

An icelandic vector object

## Examples

``` r
gregorian_date(2025, 4, 20:30) |>
  as_icelandic()
#> <icelandic[11]>
#>  [1] 2024-Win-26-Sun 2024-Win-26-Mon 2024-Win-26-Tue 2024-Win-26-Wed
#>  [5] 2025-Sum-01-Thu 2025-Sum-01-Fri 2025-Sum-01-Sat 2025-Sum-01-Sun
#>  [9] 2025-Sum-01-Mon 2025-Sum-01-Tue 2025-Sum-01-Wed
icelandic_date(2025, 1, 6, 0:6) |>
  day_of_week()
#> [1] "Sunnudagur"   "Manudagur"    "Þriðjudagur"  "Miðvikudagur" "Fimmtudagur" 
#> [6] "Føstudagur"   "Laugardagur" 
```
