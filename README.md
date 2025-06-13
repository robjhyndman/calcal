
<!-- README.md is generated from README.Rmd. Please edit that file -->

# calcal <img src="man/figures/logo.png" align="right" width = 150 />

<!-- badges: start -->

<!-- badges: end -->

The goal of calcal is to do calendrical calculations, based on the
algorithms described in [Reingold and Dershowitz (2018) *Calendrical
Calculations*, 4th edition, Cambridge University
Press](https://doi.org/10.1017/9781107415058).

It is largely a translation of the Lisp code produced by Reingold and
Dershowitz for [Calendrica
4.0](https://github.com/EdReingold/calendar-code2).

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
library(tibble)
library(tidyr)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following object is masked from 'package:calcal':
#> 
#>     location
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
tibble(
  Gregorian = gregorian_date(1990, 2, 20) + 1:10,
  ISO = as_iso(Gregorian),
  Chinese = as_chinese(Gregorian),
  Islamic = as_islamic(Gregorian),
  Hebrew = as_hebrew(Gregorian)
)
#> # A tibble: 10 × 5
#>      Gregorian        ISO    Chinese     Islamic         Hebrew
#>          <Gre>      <ISO>      <Chi>       <Hij>          <Heb>
#>  1 1990-Feb-21 1990-08-03 78-07-1-27 1410-Raj-25 5750-Shevat-26
#>  2 1990-Feb-22 1990-08-04 78-07-1-28 1410-Raj-26 5750-Shevat-27
#>  3 1990-Feb-23 1990-08-05 78-07-1-29 1410-Raj-27 5750-Shevat-28
#>  4 1990-Feb-24 1990-08-06 78-07-1-30 1410-Raj-28 5750-Shevat-29
#>  5 1990-Feb-25 1990-08-07 78-07-2-01 1410-Raj-29 5750-Shevat-30
#>  6 1990-Feb-26 1990-09-01 78-07-2-02 1410-Raj-30   5750-Adar-01
#>  7 1990-Feb-27 1990-09-02 78-07-2-03 1410-Sha-01   5750-Adar-02
#>  8 1990-Feb-28 1990-09-03 78-07-2-04 1410-Sha-02   5750-Adar-03
#>  9 1990-Mar-01 1990-09-04 78-07-2-05 1410-Sha-03   5750-Adar-04
#> 10 1990-Mar-02 1990-09-05 78-07-2-06 1410-Sha-04   5750-Adar-05
```

Some religious and cultural holidays

``` r
tibble(
  year = 2025:2028,
  # Christian holidays
   `Easter` = easter(year),
   `Orthodox Easter` = orthodox_easter(year),
   `Christmas` = christmas(year),
   `Orthodox Christmas` = orthodox_christmas(year),
  # Jewish holidays
   `Rosh Hashanah` = rosh_hashanah(year),
   `Passover` = passover(year),
   `Yom Kippur` = yom_kippur(year),
   `Sukkot` = sukkot(year),
   `Hanukkah` = hanukkah(year),
  # Islamic holidays
   `Ramadan` = ramadan(year),
   `Eid al-Fitr` = eid_al_fitr(year),
   `Eid al-Adha` = eid_al_adha(year),
  # Baháʼí holidays
   `Birth of the Bab` = birth_of_the_bab(year),
   `Naw Ruz` = naw_ruz(year),
   `Feast of Ridvan` = feast_of_ridvan(year),
  # Chinese Holidays
   `Chinese New Year` = chinese_new_year(year),
   `Dragon Festival` = dragon_festival(year),
   `Qing Ming` = qing_ming(year)
) |>
  pivot_longer(-year) |>
  pivot_wider(names_from = year, values_from = value) |>
  arrange(`2025`)
#> # A tibble: 18 × 5
#>    name                    `2025`      `2026`      `2027`      `2028`
#>    <chr>                    <Gre>       <Gre>       <Gre>       <Gre>
#>  1 Orthodox Christmas 2025-Jan-07 2026-Jan-07 2027-Jan-07 2028-Jan-07
#>  2 Chinese New Year   2025-Jan-29 2026-Feb-17 2027-Feb-06 2028-Jan-26
#>  3 Ramadan            2025-Mar-01 2026-Feb-18 2027-Feb-08 2028-Jan-28
#>  4 Naw Ruz            2025-Mar-20 2026-Mar-20 2027-Mar-21 2028-Mar-20
#>  5 Eid al-Fitr        2025-Mar-31 2026-Mar-20 2027-Mar-10 2028-Feb-27
#>  6 Qing Ming          2025-Apr-04 2026-Apr-04 2027-Apr-05 2028-Apr-04
#>  7 Passover           2025-Apr-13 2026-Apr-02 2027-Apr-22 2028-Apr-11
#>  8 Easter             2025-Apr-20 2026-Apr-05 2027-Mar-28 2028-Apr-16
#>  9 Orthodox Easter    2025-Apr-20 2026-Apr-12 2027-May-02 2028-Apr-16
#> 10 Feast of Ridvan    2025-Apr-20 2026-Apr-20 2027-Apr-21 2028-Apr-20
#> 11 Dragon Festival    2025-May-31 2026-Jun-19 2027-Jun-08 2028-May-28
#> 12 Eid al-Adha        2025-Jun-07 2026-May-27 2027-May-17 2028-May-05
#> 13 Rosh Hashanah      2025-Sep-23 2026-Sep-12 2027-Oct-02 2028-Sep-21
#> 14 Yom Kippur         2025-Oct-02 2026-Sep-21 2027-Oct-11 2028-Sep-30
#> 15 Sukkot             2025-Oct-07 2026-Sep-26 2027-Oct-16 2028-Oct-05
#> 16 Birth of the Bab   2025-Oct-22 2026-Nov-10 2027-Oct-30 2028-Oct-19
#> 17 Hanukkah           2025-Dec-15 2026-Dec-05 2027-Dec-25 2028-Dec-13
#> 18 Christmas          2025-Dec-25 2026-Dec-25 2027-Dec-25 2028-Dec-25
```
