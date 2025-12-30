# Balinese Pawukon calendar dates

The Balinese calendar repeats every 210 days. It has 10 concurrent
weeks, of lengths 1, 2, ..., 10 days. The 210 day cycles are unnumbered,
so there is no way to convert a Balinese date into a unique date on
another calendar.

## Usage

``` r
balinese_date(
  luang = integer(),
  dwiwara = integer(),
  triwara = integer(),
  caturwara = integer(),
  pancawara = integer(),
  sadwara = integer(),
  saptawara = integer(),
  asatawara = integer(),
  sangawara = integer(),
  dasawara = integer()
)

as_balinese(date)
```

## Arguments

- luang:

  A numeric vector

- dwiwara:

  A numeric vector

- triwara:

  A numeric vector

- caturwara:

  A numeric vector

- pancawara:

  A numeric vector

- sadwara:

  A numeric vector

- saptawara:

  A numeric vector

- asatawara:

  A numeric vector

- sangawara:

  A numeric vector

- dasawara:

  A numeric vector

- date:

  A vector of dates on some calendar.

## Value

A balinese vector object

## See also

[kajeng_keliwon](https://pkg.robjhyndman.com/calcal/reference/kajeng_keliwon.md)

## Examples

``` r
gregorian_date(2025,6,1:10) |>
  as_balinese()
#> <balinese[10]>
#>  [1] TRUE-02-02-03-04-05-01-07-02-00  FALSE-01-03-04-05-06-02-08-03-03
#>  [3] FALSE-01-01-01-01-01-03-01-04-09 FALSE-01-02-02-02-02-04-02-05-07
#>  [5] TRUE-02-03-03-03-03-05-03-06-06  FALSE-01-01-04-04-04-06-04-07-01
#>  [7] TRUE-02-02-01-05-05-07-05-08-08  FALSE-01-03-02-01-06-01-06-09-01
#>  [9] TRUE-02-01-03-02-01-02-07-01-04  FALSE-01-02-04-03-02-03-08-02-01
```
