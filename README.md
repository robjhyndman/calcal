
<!-- README.md is generated from README.Rmd. Please edit that file -->

# calcal <img src="man/figures/calcal-hex.png" align="right" width = 150 />

<!-- badges: start -->
<!-- badges: end -->

The goal of calcal is to do calendrical calculations, based on the
algorithms described in [Reingold and Dershowitz (2018) *Calendrical
Calculations*, 4th edition, Cambridge University
Press](https://doi.org/10.1017/9781107415058).

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
#> 
#> Attaching package: 'calcal'
#> The following object is masked from 'package:base':
#> 
#>     julian
today <- Sys.Date()
as_rd(today)
#> <rd_fixed[1]>
#> [1] 739362
as_gregorian(today)
#> <gregorian[1]>
#> [1] 2025-04-21
```

Some US holidays

``` r
c(
  us_independence_day(2025),
  us_labor_day(2025),
  us_memorial_day(2025),
  us_election_day(2025),
  us_daylight_saving_start(2025),
  us_daylight_saving_end(2025),
  christmas(2025),
  advent(2025),
  epiphany(2025)
)
#> <gregorian[9]>
#> [1] 2025-07-04 2025-09-01 2025-05-26 2025-11-04 2025-04-06 2025-10-26 2025-12-25
#> [8] 2025-11-30 2025-01-05
```
