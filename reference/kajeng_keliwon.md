# Balinese special days

Find all occurrences of Kajeng Keliwon and Tumpek in a vector of
Gregorian years.

## Usage

``` r
kajeng_keliwon(year)

tumpek(year)
```

## Arguments

- year:

  A numeric vector of Gregorian years

## Value

A vector of dates on the Gregorian calendar

## See also

[balinese_date](https://pkg.robjhyndman.com/calcal/reference/balinese_date.md)

## Examples

``` r
kajeng_keliwon(2025)
#> <gregorian[25]>
#>  [1] 2025-Jan-03 2025-Jan-18 2025-Feb-02 2025-Feb-17 2025-Mar-04 2025-Mar-19
#>  [7] 2025-Apr-03 2025-Apr-18 2025-May-03 2025-May-18 2025-Jun-02 2025-Jun-17
#> [13] 2025-Jul-02 2025-Jul-17 2025-Aug-01 2025-Aug-16 2025-Aug-31 2025-Sep-15
#> [19] 2025-Sep-30 2025-Oct-15 2025-Oct-30 2025-Nov-14 2025-Nov-29 2025-Dec-14
#> [25] 2025-Dec-29
tumpek(2025)
#> <gregorian[10]>
#>  [1] 2025-Jan-18 2025-Feb-22 2025-Mar-29 2025-May-03 2025-Jun-07 2025-Jul-12
#>  [7] 2025-Aug-16 2025-Sep-20 2025-Oct-25 2025-Nov-29
```
