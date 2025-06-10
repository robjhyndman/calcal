test_that("granulaities", {
  yr2025 <- gregorian_date(2025, 1, 1) + 0:364
  month <- function(date) {
    y <- year(date)
    (y - min(y)) * 12 + month_of_year(date)
  }
  year_month <- linear_cyclic(year, month_of_year, month.abb, "year-month")
  month_day <- linear_cyclic(
    month,
    day_of_month,
    paste(1:31),
    "month-day",
    periodic = FALSE
  )
  expect_error(month_day(yr2025) + 1)
  expect_equal(
    as.character(year_month(yr2025)),
    gsub(" ", "-", as.character(tsibble::yearmonth(as.Date(yr2025))))
  )
  expect_equal(year_month(yr2025) + 1, 1 + year_month(yr2025))
  expect_equal(
    year_month(yr2025) |> as.numeric(),
    c(2025 + month_of_year(yr2025) / 12)
  )
})
