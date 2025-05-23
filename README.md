
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
  Gregorian = gregorian_date(1990, 2, 20) + 1:10,
  Date = as.Date(Gregorian),
  RD = as_rd(Gregorian),
  ISO = as_iso(Gregorian),
  Julian = as_julian(Gregorian),
  Roman = as_roman(Gregorian)
)
#> # A tibble: 10 × 6
#>     Gregorian Date           RD        ISO     Julian                    Roman
#>         <Gre> <date>       <RD>      <iso>      <Jul>                    <Rom>
#>  1 1990-02-21 1990-02-21 726519 1990-08-03 1990-02-08      1990-Feb-ad_vi_Ides
#>  2 1990-02-22 1990-02-22 726520 1990-08-04 1990-02-09       1990-Feb-ad_v_Ides
#>  3 1990-02-23 1990-02-23 726521 1990-08-05 1990-02-10      1990-Feb-ad_iv_Ides
#>  4 1990-02-24 1990-02-24 726522 1990-08-06 1990-02-11     1990-Feb-ad_iii_Ides
#>  5 1990-02-25 1990-02-25 726523 1990-08-07 1990-02-12     1990-Feb-pridie_Ides
#>  6 1990-02-26 1990-02-26 726524 1990-09-01 1990-02-13            1990-Feb-Ides
#>  7 1990-02-27 1990-02-27 726525 1990-09-02 1990-02-14  1990-Mar-ad_xvi_Kalends
#>  8 1990-02-28 1990-02-28 726526 1990-09-03 1990-02-15   1990-Mar-ad_xv_Kalends
#>  9 1990-03-01 1990-03-01 726527 1990-09-04 1990-02-16  1990-Mar-ad_xiv_Kalends
#> 10 1990-03-02 1990-03-02 726528 1990-09-05 1990-02-17 1990-Mar-ad_xiii_Kalends
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
