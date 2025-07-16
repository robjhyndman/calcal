
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

You can install the **stable** version from
[CRAN](https://CRAN.R-project.org/package=calcal) with:

``` r
pak::pak("calcal")
```

You can install the **development** version from
[GitHub](https://github.com/) with:

``` r
pak::pak("robjhyndman/calcal")
```

## Example

Dates in multiple calendars:

``` r
library(calcal)
library(tibble)
library(tidyr)
library(dplyr)
tibble(
  Gregorian = gregorian_date(1990, 2, 20) + 1:10,
  ISO = as_iso(Gregorian),
  Islamic = as_islamic(Gregorian),
  Hebrew = as_hebrew(Gregorian)
)
#> # A tibble: 10 × 4
#>      Gregorian        ISO     Islamic         Hebrew
#>          <Gre>      <ISO>       <Hij>          <Heb>
#>  1 1990-Feb-21 1990-08-03 1410-Raj-25 5750-Shevat-26
#>  2 1990-Feb-22 1990-08-04 1410-Raj-26 5750-Shevat-27
#>  3 1990-Feb-23 1990-08-05 1410-Raj-27 5750-Shevat-28
#>  4 1990-Feb-24 1990-08-06 1410-Raj-28 5750-Shevat-29
#>  5 1990-Feb-25 1990-08-07 1410-Raj-29 5750-Shevat-30
#>  6 1990-Feb-26 1990-09-01 1410-Raj-30   5750-Adar-01
#>  7 1990-Feb-27 1990-09-02 1410-Sha-01   5750-Adar-02
#>  8 1990-Feb-28 1990-09-03 1410-Sha-02   5750-Adar-03
#>  9 1990-Mar-01 1990-09-04 1410-Sha-03   5750-Adar-04
#> 10 1990-Mar-02 1990-09-05 1410-Sha-04   5750-Adar-05
```

Some religious and cultural holidays

``` r
tibble(
  year = 2025:2028,
  # Christian holidays
    Easter = easter(year),
   `Orthodox Easter` = orthodox_easter(year),
    Christmas = christmas(year),
   `Orthodox Christmas` = orthodox_christmas(year),
  # Jewish holidays
   `Rosh Hashanah` = rosh_hashanah(year),
    Passover = passover(year),
   `Yom Kippur` = yom_kippur(year),
    Sukkot = sukkot(year),
    Hanukkah = hanukkah(year),
  # Islamic holidays
    Ramadan = ramadan(year),
   `Eid al-Fitr` = eid_al_fitr(year),
   `Eid al-Adha` = eid_al_adha(year),
  # Chinese Holidays
   `Chinese New Year` = chinese_new_year(year),
   `Dragon Festival` = dragon_festival(year),
   `Qing Ming` = qing_ming(year),
  # Hindu Holidays
   `Hindu Lunar New Year` = hindu_lunar_new_year(year),
   `Hindu Solar New Year` = mesha_sankranti(year),
    Diwali = diwali(year),
    Rama = rama(year),
    Shiva = shiva(year)
) |>
  pivot_longer(-year) |>
  pivot_wider(names_from = year, values_from = value) |>
  arrange(`2025`)
#> # A tibble: 20 × 5
#>    name                      `2025`      `2026`      `2027`      `2028`
#>    <chr>                      <Gre>       <Gre>       <Gre>       <Gre>
#>  1 Orthodox Christmas   2025-Jan-07 2026-Jan-07 2027-Jan-07 2028-Jan-07
#>  2 Chinese New Year     2025-Jan-29 2026-Feb-17 2027-Feb-06 2028-Jan-26
#>  3 Shiva                2025-Feb-27 2026-Feb-16 2027-Mar-07 2028-Feb-24
#>  4 Ramadan              2025-Mar-01 2026-Feb-18 2027-Feb-08 2028-Jan-28
#>  5 Hindu Lunar New Year 2025-Mar-30 2026-Mar-19 2027-Apr-07 2028-Mar-27
#>  6 Eid al-Fitr          2025-Mar-31 2026-Mar-20 2027-Mar-10 2028-Feb-27
#>  7 Qing Ming            2025-Apr-04 2026-Apr-05 2027-Apr-05 2028-Apr-04
#>  8 Rama                 2025-Apr-06 2026-Mar-27 2027-Apr-15 2028-Apr-04
#>  9 Passover             2025-Apr-13 2026-Apr-02 2027-Apr-22 2028-Apr-11
#> 10 Hindu Solar New Year 2025-Apr-14 2026-Apr-14 2027-Apr-14 2028-Apr-13
#> 11 Easter               2025-Apr-20 2026-Apr-05 2027-Mar-28 2028-Apr-16
#> 12 Orthodox Easter      2025-Apr-20 2026-Apr-12 2027-May-02 2028-Apr-16
#> 13 Dragon Festival      2025-May-31 2026-Jun-19 2027-Jun-09 2028-May-28
#> 14 Eid al-Adha          2025-Jun-07 2026-May-27 2027-May-17 2028-May-05
#> 15 Rosh Hashanah        2025-Sep-23 2026-Sep-12 2027-Oct-02 2028-Sep-21
#> 16 Yom Kippur           2025-Oct-02 2026-Sep-21 2027-Oct-11 2028-Sep-30
#> 17 Sukkot               2025-Oct-07 2026-Sep-26 2027-Oct-16 2028-Oct-05
#> 18 Diwali               2025-Oct-22 2026-Nov-10 2027-Oct-30 2028-Nov-17
#> 19 Hanukkah             2025-Dec-15 2026-Dec-05 2027-Dec-25 2028-Dec-13
#> 20 Christmas            2025-Dec-25 2026-Dec-25 2027-Dec-25 2028-Dec-25
```
