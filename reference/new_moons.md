# Full moons and new moons in Gregorian years

Calculate all the near-full or near-new moons in a vector of Gregorian
years

## Usage

``` r
new_moons(year)

full_moons(year)
```

## Arguments

- year:

  A vector of Gregorian years

## Value

A vector of Gregorian dates representing the full moons or new moons in
the given years.

A vector of dates

## Examples

``` r
full_moons(2025)
#> <gregorian[12]>
#>  [1] 2025-Jan-14 2025-Feb-13 2025-Mar-14 2025-Apr-13 2025-May-12 2025-Jun-10
#>  [7] 2025-Jul-10 2025-Aug-08 2025-Sep-07 2025-Oct-06 2025-Nov-05 2025-Dec-05
new_moons(2025)
#> <gregorian[12]>
#>  [1] 2025-Jan-29 2025-Feb-28 2025-Mar-29 2025-Apr-27 2025-May-27 2025-Jun-25
#>  [7] 2025-Jul-24 2025-Aug-23 2025-Sep-21 2025-Oct-21 2025-Nov-20 2025-Dec-20
```
