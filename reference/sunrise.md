# Sun and moon rise and set given a date and location

Calculate the time of sunrise, sunset, moonrise and moonset at a
specific location and date. The time zone of the location is used as
specified in the `location` object. No adjustments are made for daylight
saving.

## Usage

``` r
sunrise(date, location)

sunset(date, location)

moonset(date, location)

moonrise(date, location)
```

## Arguments

- date:

  Vector of dates on some calendar.

- location:

  Vector of locations of class "location", usually the output from the
  `location` function

## Value

Time of sunrise

## Examples

``` r
melbourne <- location(-37.8136, 144.9631, 31, 10)
sydney <- location(-33.8688, 151.2093, 3, 10)
sunrise(gregorian_date(2025, 1, 1), c(melbourne, sydney))
#> <time_of_day[2]>
#> [1] 05:00:25.00 04:47:19.90
sunset(gregorian_date(2025, 1, 1), c(melbourne, sydney))
#> <time_of_day[2]>
#> [1] 19:46:39.75 19:09:47.84
moonrise(gregorian_date(2025, 1, 1), c(melbourne, sydney))
#> <time_of_day[2]>
#> [1] 05:42:37.06 05:30:21.65
moonset(gregorian_date(2025, 1, 1), c(melbourne, sydney))
#> <time_of_day[2]>
#> [1] 21:11:26.75 20:32:28.22
```
