# Lunar phase at date

Lunar phase at date, as an angle in degrees. An angle of 0 means a new
moon, 90 degrees means the first quarter, 180 means a full moon, and 270
degrees means the last quarter.

## Usage

``` r
lunar_phase(date)
```

## Arguments

- date:

  Date vector

## Value

A numeric vector of angles in degrees representing the lunar phase at
the given dates.

## Examples

``` r
april2025 <- gregorian_date(2025, 4, 1:30)
lunar_phase(april2025)
#>  [1]  35.87571  49.68103  63.14151  76.19938  88.83943 101.08071 112.96536
#>  [8] 124.54784 135.88624 147.03637 158.04870 168.96764 179.83223 190.67757
#> [15] 201.53670 212.44314 223.43391 234.55221 245.84850 257.37951 269.20513
#> [22] 281.38360 293.96466 306.97947 320.42760 334.26406 348.39227   2.66872
#> [29]  16.92140  30.97752
```
