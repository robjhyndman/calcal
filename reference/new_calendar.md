# Define calendar objects

Generate a calendar object of class "calendar". Examples of calendars
produced in this way include `cal_chinese`, `cal_gregorian`,
`cal_hebrew`, `cal_islamic`, and `cal_iso`.

## Usage

``` r
new_calendar(
  name,
  short_name,
  granularities,
  validate_granularities,
  format,
  from_rd,
  to_rd
)

cal_babylonian

cal_bahai

cal_balinese

cal_chinese

cal_japanese

cal_korean

cal_vietnamese

cal_coptic

cal_ethiopic

cal_egyptian

cal_armenian

cal_french

cal_afrench

cal_gregorian

cal_hebrew

cal_icelandic

cal_islamic

cal_iso

cal_julian

cal_oislamic

cal_saudi

cal_ohebrew

cal_samaritan

cal_mayan

cal_hindu_lunar

cal_hindu_solar

cal_old_hindu_solar

cal_old_hindu_lunar

cal_persian

cal_apersian

cal_roman

cal_tibetan
```

## Arguments

- name:

  Name of calendar

- short_name:

  Short name of calendar

- granularities:

  Character vector with names of granularities of calendar (e.g., for
  the Gregorian calendar, the granularities are `year`, `month`, and
  `day`).

- validate_granularities:

  Function to check granularities are valid (e.g., Gregorian months
  should be between 1 and 12).

- format:

  Function to specify date format as a character string.

- from_rd:

  Function to convert from RD to calendar date.

- to_rd:

  Function to convert from calendar date to RD.

## Value

A calendar object of class "calendar"

## Examples

``` r
cal_gregorian
#> Calendar: gregorian
#> Granularities: year, month, day
tibble::tibble(
  x = new_date(year = 2025, month = 5, day = 1:31, calendar = cal_gregorian),
  y = as_date(x, calendar = cal_islamic)
)
#> # A tibble: 31 × 2
#>              x               y
#>          <Gre>           <Hij>
#>  1 2025-May-01 1446-Dhu'l_Q-03
#>  2 2025-May-02 1446-Dhu'l_Q-04
#>  3 2025-May-03 1446-Dhu'l_Q-05
#>  4 2025-May-04 1446-Dhu'l_Q-06
#>  5 2025-May-05 1446-Dhu'l_Q-07
#>  6 2025-May-06 1446-Dhu'l_Q-08
#>  7 2025-May-07 1446-Dhu'l_Q-09
#>  8 2025-May-08 1446-Dhu'l_Q-10
#>  9 2025-May-09 1446-Dhu'l_Q-11
#> 10 2025-May-10 1446-Dhu'l_Q-12
#> # ℹ 21 more rows
```
