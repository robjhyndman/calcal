# Mayan calendar dates

There are three Mayan calendars: the famous "long count" calendar, the
"Haab" calendar, and the "Tzolkin" calendar. Of these, only the long
count calendar can be converted to and from other calendars, so it is
the only one that has been implemented here.

## Usage

``` r
mayan_date(
  baktun = integer(),
  katun = integer(),
  tun = integer(),
  uinal = integer(),
  kin = integer()
)

as_mayan(date)
```

## Arguments

- baktun:

  Numeric vector

- katun:

  Numeric vector

- tun:

  Numeric vector

- uinal:

  Numeric vector

- kin:

  Numeric vector

- date:

  Vector of dates on some calendar

## Value

A mayan vector object

## Details

The Mayan long count calendar is a vigesimal (base-20) calendar with
five components: kin (1 day), uinal (20 kin), tun (18 uinal), katun (20
tun), and baktun (20 katun). So the full cycle repeats every 20x18x20x20
= 144,000 days (approximately 394 years).

## See also

[cal_mayan](https://pkg.robjhyndman.com/calcal/reference/new_calendar.md)

## Examples

``` r
gregorian_date(2012, 12, 10:30) |>
  as_mayan()
#> <mayan[21]>
#>  [1] 12-19-17-19-09 12-19-17-19-10 12-19-17-19-11 12-19-17-19-12 12-19-17-19-13
#>  [6] 12-19-17-19-14 12-19-17-19-15 12-19-17-19-16 12-19-17-19-17 12-19-17-19-18
#> [11] 12-19-17-19-19 13-00-00-00-00 13-00-00-00-01 13-00-00-00-02 13-00-00-00-03
#> [16] 13-00-00-00-04 13-00-00-00-05 13-00-00-00-06 13-00-00-00-07 13-00-00-00-08
#> [21] 13-00-00-00-09
```
