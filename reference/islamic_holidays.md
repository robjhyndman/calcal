# Islamic holidays

Functions to return Gregorian dates for various Islamic holidays.
Specific dates can vary slightly based on moon sightings in different
regions.

## Usage

``` r
islamic_new_year(year)

mawlid(year)

ramadan(year)

eid_al_fitr(year)

eid_al_adha(year)
```

## Arguments

- year:

  A numeric vector of Gregorian years

## Value

A vector of dates on the Gregorian calendar

## See also

[islamic_date](https://pkg.robjhyndman.com/calcal/reference/islamic.md)

## Examples

``` r
tibble::tibble(
  year = 2025:2029,
  `New year` = islamic_new_year(year),
  Mawlid = mawlid(year),
  Ramadan = ramadan(year),
  `Eid al-Fitr` = eid_al_fitr(year),
  `Eid al-Adha` = eid_al_adha(year)
)
#> # A tibble: 5 × 6
#>    year  `New year`      Mawlid     Ramadan `Eid al-Fitr` `Eid al-Adha`
#>   <int>       <Gre>       <Gre>       <Gre>         <Gre>         <Gre>
#> 1  2025 2025-Jun-27 2025-Aug-07 2025-Mar-01   2025-Mar-31   2025-Jun-07
#> 2  2026 2026-Jun-17 2026-Jul-28 2026-Feb-18   2026-Mar-20   2026-May-27
#> 3  2027 2027-Jun-06 2027-Jul-17 2027-Feb-08   2027-Mar-10   2027-May-17
#> 4  2028 2028-May-25 2028-Jul-05 2028-Jan-28   2028-Feb-27   2028-May-05
#> 5  2029 2029-May-15 2029-Jun-25 2029-Jan-16   2029-Feb-15   2029-Apr-24
ramadan(2030)
#> <gregorian[2]>
#> [1] 2030-Jan-06 2030-Dec-26
```
