# Jewish Holidays

Functions to return Gregorian dates for various Jewish holidays

## Usage

``` r
yom_kippur(year)

passover(year)

purim(year)

ta_anit_esther(year)

tishah_be_av(year)

hanukkah(year)

rosh_hashanah(year)

sukkot(year)

shavuot(year)
```

## Arguments

- year:

  A numeric vector of Gregorian years

## Value

A vector of dates on the Gregorian calendar

## See also

[hebrew_date](https://pkg.robjhyndman.com/calcal/reference/hebrew.md)

## Examples

``` r
tibble::tibble(
  year = 2025:2030,
  ta_anit_esther = ta_anit_esther(year),
  purim = purim(year),
  passover = passover(year),
  shavuot = shavuot(year),
  tishah_be_av = tishah_be_av(year),
  rosh_hashanah = rosh_hashanah(year),
  yom_kippur = yom_kippur(year),
  sukkot = sukkot(year),
  hanukkah = hanukkah(year)
)
#> # A tibble: 6 × 10
#>    year ta_anit_esther       purim    passover     shavuot tishah_be_av
#>   <int>          <Gre>       <Gre>       <Gre>       <Gre>        <Gre>
#> 1  2025    2025-Mar-13 2025-Mar-14 2025-Apr-13 2025-Jun-02  2025-Aug-03
#> 2  2026    2026-Mar-02 2026-Mar-03 2026-Apr-02 2026-May-22  2026-Jul-23
#> 3  2027    2027-Mar-22 2027-Mar-23 2027-Apr-22 2027-Jun-11  2027-Aug-12
#> 4  2028    2028-Mar-09 2028-Mar-12 2028-Apr-11 2028-May-31  2028-Aug-01
#> 5  2029    2029-Feb-28 2029-Mar-01 2029-Mar-31 2029-May-20  2029-Jul-22
#> 6  2030    2030-Mar-18 2030-Mar-19 2030-Apr-18 2030-Jun-07  2030-Aug-08
#> # ℹ 4 more variables: rosh_hashanah <Gre>, yom_kippur <Gre>, sukkot <Gre>,
#> #   hanukkah <Gre>
```
