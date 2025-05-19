
<!-- README.md is generated from README.Rmd. Please edit that file -->

# calcal <img src="man/figures/calcal-hex.png" align="right" width = 150 />

<!-- badges: start -->

<!-- badges: end -->

The goal of calcal is to do calendrical calculations, based on the
algorithms described in [Reingold and Dershowitz (2018) *Calendrical
Calculations*, 4th edition, Cambridge University
Press](https://doi.org/10.1017/9781107415058).

It is a translation of the Lisp code produced by Reingold and Dershowitz
for [Calendrica 4.0](https://github.com/EdReingold/calendar-code2).

## Installation

You can install the development version of calcal from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("robjhyndman/calcal")
```

## Example

Dates in multiple calendars:

``` r
library(calcal)
tibble::tibble(
  dates = seq(as.Date("1900-02-27"), length = 15, by = "1 day"),
  RD = as_rd(dates),
  Gregorian = as_gregorian(dates),
  ISO = as_iso(dates),
  Julian = as_julian(dates),
  Roman = as_roman(dates)
)
#> # A tibble: 15 × 6
#>    dates          RD  Gregorian        ISO     Julian                    Roman
#>    <date>       <RD>      <Gre>      <iso>      <Jul>                    <Rom>
#>  1 1900-02-27 693653 1900-02-27 1900-09-02 1900-02-15   1900-Mar-ad_xv_Kalends
#>  2 1900-02-28 693654 1900-02-28 1900-09-03 1900-02-16  1900-Mar-ad_xiv_Kalends
#>  3 1900-03-01 693655 1900-03-01 1900-09-04 1900-02-17 1900-Mar-ad_xiii_Kalends
#>  4 1900-03-02 693656 1900-03-02 1900-09-05 1900-02-18  1900-Mar-ad_xii_Kalends
#>  5 1900-03-03 693657 1900-03-03 1900-09-06 1900-02-19   1900-Mar-ad_xi_Kalends
#>  6 1900-03-04 693658 1900-03-04 1900-09-07 1900-02-20    1900-Mar-ad_x_Kalends
#>  7 1900-03-05 693659 1900-03-05 1900-10-01 1900-02-21   1900-Mar-ad_ix_Kalends
#>  8 1900-03-06 693660 1900-03-06 1900-10-02 1900-02-22 1900-Mar-ad_viii_Kalends
#>  9 1900-03-07 693661 1900-03-07 1900-10-03 1900-02-23  1900-Mar-ad_vii_Kalends
#> 10 1900-03-08 693662 1900-03-08 1900-10-04 1900-02-24   1900-Mar-ad_vi_Kalends
#> 11 1900-03-09 693663 1900-03-09 1900-10-05 1900-02-25   1900-Mar-ad_vi_Kalends
#> 12 1900-03-10 693664 1900-03-10 1900-10-06 1900-02-26    1900-Mar-ad_v_Kalends
#> 13 1900-03-11 693665 1900-03-11 1900-10-07 1900-02-27   1900-Mar-ad_iv_Kalends
#> 14 1900-03-12 693666 1900-03-12 1900-11-01 1900-02-28  1900-Mar-ad_iii_Kalends
#> 15 1900-03-13 693667 1900-03-13 1900-11-02 1900-02-29  1900-Mar-pridie_Kalends
```

Some US holidays

``` r
c(
  us_memorial_day(2025),
  us_independence_day(2025),
  us_labor_day(2025),
  us_election_day(2025),
  us_daylight_saving_start(2025),
  us_daylight_saving_end(2025)
)
#> <gregorian[6]>
#> [1] 2025-05-26 2025-07-04 2025-09-01 2025-11-04 2025-03-09 2025-11-02
```

Some Christian holidays

``` r
c(
  easter(2025),
  orthodox_easter(2025),
  alt_orthodox_easter(2025),
  pentecost(2025),
  advent(2025),
  christmas(2025),
  epiphany(2026),
  orthodox_christmas(2026)
)
#> <gregorian[8]>
#> [1] 2025-04-20 2025-04-20 2025-04-20 2025-06-08 2025-11-30 2025-12-25 2026-01-04
#> [8] 2026-01-07
```
