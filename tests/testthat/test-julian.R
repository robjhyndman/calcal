test_that("julian", {
  expect_equal(julian_date(2025, 4, 19), as_julian("2025-05-02"))
  expect_equal(
    as.Date(as_gregorian(julian_date(2025, 4, 19))),
    as.Date("2025-04-19") + 13
  )
  expect_equal(
    vec_data(julian_date(2025, 4, 19)),
    vec_data(as_gregorian("2025-05-02"))
  )
  expect_error(julian_date(2025, 13, 1))
  expect_error(julian_date(2025, 2, 30))
  expect_error(julian_date(2025, 2, 29))
  expect_error(julian_date(2024.4, 2, 12))
  expect_no_error(julian_date(2025, 2, 28))
  expect_no_error(julian_date(2020, 2, 29))
  expect_no_error(julian_date(2000, 2, 29))
  expect_no_error(julian_date(1900, 2, 29))
  expect_error(julian_date(1:3, 2:4, 5:6))
  expect_no_error(julian_date(1:3, 2:4, 5))
  expect_no_error(julian_date(NA, NA, 31))
  expect_no_error(julian_date(NA, NA, NA))
})
