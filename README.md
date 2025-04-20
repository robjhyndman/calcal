
<!-- README.md is generated from README.Rmd. Please edit that file -->

# calcal

<!-- badges: start -->
<!-- badges: end -->

The goal of calcal is to do calendrical calculations, based on the
algorithms described in Reingold and Dershowitz (2018)
<doi:10.1017/9781107415058>.

## Installation

You can install the development version of calcal from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("robjhyndman/calcal")
```

## Example

Today’s date in multiple calendars:

``` r
library(calcal)
today <- Sys.Date()
as_rd(today)
#> [1] "RD 739361"
as_gregorian(today)
#> [1] "G2025-04-20"
as_julian(today)
#> [1] "J2025-04-07"
```
