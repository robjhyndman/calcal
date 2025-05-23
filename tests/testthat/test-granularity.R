test_that("granulaities", {
  yr2025 <- gregorian_date(2025, 1, 1) + 0:364
  year_month <- linear_cyclic(year, month_of_year, month.abb, "year-month")
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
