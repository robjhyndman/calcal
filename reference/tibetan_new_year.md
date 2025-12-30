# Tibetan holidays

The Tibetan New Year occurs on the first day of the Tibetan calendar.
These functions calculate the date given either a Gregorian year or a
Tibetan year. Both return a Gregorian date.

## Usage

``` r
tibetan_new_year(year)

losar(t_year)
```

## Arguments

- year:

  A vector of Gregorian years

- t_year:

  A vector of Tibetan years

## Value

A vector of Gregorian dates corresponding to the Tibetan New Year

## See also

[tibetan_date](https://pkg.robjhyndman.com/calcal/reference/tibetan_date.md)

## Examples

``` r
tibetan_new_year(2025:2028)
#> <gregorian[4]>
#> [1] 2025-Feb-28 2026-Feb-18 2027-Feb-07 2028-Feb-26
losar(2152:2154)
#> <gregorian[3]>
#> [1] 2025-Feb-28 2026-Feb-18 2027-Feb-07
```
