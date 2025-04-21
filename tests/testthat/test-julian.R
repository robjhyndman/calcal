test_that("julian", {
  expect_equal(julian(2025, 4, 19), as_julian("2025-05-02"))
  expect_equal(
    as.Date(as_gregorian(julian(2025, 4, 19))),
    as.Date("2025-04-19") + 13
  )
  expect_equal(as_rd(julian(2025, 4, 19)), as_rd("2025-05-02"))
  expect_error(julian(2025, 13, 1))
  expect_error(julian(2025, 2, 30))
  expect_error(julian(2025, 2, 29))
  expect_error(julian(2024.4, 2, 12))
  expect_no_error(julian(2025, 2, 28))
  expect_no_error(julian(2020, 2, 29))
  expect_no_error(julian(2000, 2, 29))
  expect_no_error(julian(1900, 2, 29))
  expect_no_error(as_julian(as_rd(1:100)))
  expect_error(julian(1:3, 2:4, 5:6))
  expect_no_error(julian(1:3, 2:4, 5))
  expect_error(julian(NA, 2, 30))
  expect_no_error(julian(NA, NA, 31))
  expect_error(julian(NA, NA, 32))
  expect_no_error(julian(NA, NA, NA))
  expect_no_error(as_julian(as_rd(NA_integer_)))
})
