# French Revolutionary calendar dates

There are two versions of the French Revolutionary Calendar. The
original version, used from 1793, was kept in sync with the solar year
by setting the first day of Vendemiaire to the autumnal equinox. The
second version, proposed in 1795, was a simpler arithmetic calendar, but
was never used. We distinguish the two by using "afrench" (for
Arithmetic French) for the second form.

## Usage

``` r
french_date(year = integer(), month = integer(), day = integer())

afrench_date(year = integer(), month = integer(), day = integer())

as_french(date)

as_afrench(date)
```

## Arguments

- year:

  year

- month:

  month

- day:

  day

- date:

  A vector of dates on some calendar

## Value

A vector of dates on the French Revolutionary calendar

## Examples

``` r
french_date(1, 1, 1:15) |>
  as_gregorian()
#> <gregorian[15]>
#>  [1] 1792-Sep-22 1792-Sep-23 1792-Sep-24 1792-Sep-25 1792-Sep-26 1792-Sep-27
#>  [7] 1792-Sep-28 1792-Sep-29 1792-Sep-30 1792-Oct-01 1792-Oct-02 1792-Oct-03
#> [13] 1792-Oct-04 1792-Oct-05 1792-Oct-06
french_date(1, 1, 1:15) |>
  day_of_week()
#>  [1] "Primidi"  "Duodi"    "Tridi"    "Quartidi" "Quintidi" "Sextidi" 
#>  [7] "Septidi"  "Octidi"   "Nonidi"   "Decadi"   "Primidi"  "Duodi"   
#> [13] "Tridi"    "Quartidi" "Quintidi"
```
