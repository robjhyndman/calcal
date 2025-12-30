# Create a new date vector or convert a date vector to a new calendar

New dates can be calculated using `new_date()` for any calendar. Dates
can be converted from one calendar to another using `as_date()`.
`as_date()` also works with the native R `Date` class and several other
classes. When applied to integers, the conversion is from the RD day
number (with day 1 being 01-01-01 on the Gregorian calendar).

## Usage

``` r
as_date(date, calendar)

new_date(..., calendar)
```

## Arguments

- date:

  Date vector on some calendar

- calendar:

  Target calendar of class "calendar"

- ...:

  Named arguments denoting the granularities required for `calendar`.

## Value

A date vector of class "rdvec" with the specified calendar.

## Examples

``` r
april2025 <- new_date(year = 2025, month = 4, day = 1:30, calendar = cal_gregorian)
as_date(april2025, calendar = cal_iso)
#> <iso[30]>
#>  [1] 2025-14-02 2025-14-03 2025-14-04 2025-14-05 2025-14-06 2025-14-07
#>  [7] 2025-15-01 2025-15-02 2025-15-03 2025-15-04 2025-15-05 2025-15-06
#> [13] 2025-15-07 2025-16-01 2025-16-02 2025-16-03 2025-16-04 2025-16-05
#> [19] 2025-16-06 2025-16-07 2025-17-01 2025-17-02 2025-17-03 2025-17-04
#> [25] 2025-17-05 2025-17-06 2025-17-07 2025-18-01 2025-18-02 2025-18-03
```
