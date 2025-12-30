# Locations

Create a location object. These are used for calculating the timing of
astronomical events such as sunrise and sunset.

## Usage

``` r
location(
  latitude = numeric(),
  longitude = numeric(),
  elevation = numeric(),
  zone = numeric()
)
```

## Arguments

- latitude:

  A numeric vector of latitudes

- longitude:

  A numeric vector of longitudes

- elevation:

  A numeric vector of elevations above sea level (in metres)

- zone:

  A numeric vector of time zones (in hours, relative to UTC)

## Value

A location vector object

## Examples

``` r
melbourne <- location(-37.8136, 144.9631, 31, 10)
sunrise("2025-01-01", melbourne)
#> <time_of_day[1]>
#> [1] 05:00:25.00
```
