# Convert to time of day

Convert to time of day

## Usage

``` r
as_time_of_day(x, ...)
```

## Arguments

- x:

  Vector of times

- ...:

  Additional arguments not currently used

## Value

A vector containing "time_of_day" objects

## See also

[time_of_day](https://pkg.robjhyndman.com/calcal/reference/time_of_day.md)

## Examples

``` r
as_time_of_day(Sys.time())
#> <time_of_day[1]>
#> [1] 00:26:57.57
```
