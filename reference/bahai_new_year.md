# Bahá'í holidays

Dates are returned as Gregorian dates

## Usage

``` r
bahai_new_year(year)

naw_ruz(year)

feast_of_ridvan(year)

birth_of_the_bab(year)
```

## Arguments

- year:

  The year on the Gregorian calendar

## Value

A vector of dates on the Gregorian calendar

## See also

[bahai_date](https://pkg.robjhyndman.com/calcal/reference/bahai.md)

## Examples

``` r
tibble::tibble(
  year = 2025:2030,
  new_year = bahai_new_year(year),
  naw_ruz =naw_ruz(year),
  ridvan = feast_of_ridvan(year),
  birth_bab = birth_of_the_bab(year)
)
#> # A tibble: 6 × 5
#>    year    new_year     naw_ruz      ridvan   birth_bab
#>   <int>       <Gre>       <Gre>       <Gre>       <Gre>
#> 1  2025 2025-Mar-21 2025-Mar-20 2025-Apr-20 2025-Oct-22
#> 2  2026 2026-Mar-21 2026-Mar-20 2026-Apr-20 2026-Nov-10
#> 3  2027 2027-Mar-21 2027-Mar-21 2027-Apr-21 2027-Oct-30
#> 4  2028 2028-Mar-21 2028-Mar-20 2028-Apr-20 2028-Oct-19
#> 5  2029 2029-Mar-21 2029-Mar-20 2029-Apr-20 2029-Nov-07
#> 6  2030 2030-Mar-21 2030-Mar-20 2030-Apr-20 2030-Oct-28
```
