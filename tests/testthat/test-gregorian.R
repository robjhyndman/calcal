test_that("gregorian", {
  expect_equal(gregorian(2025, 1, 1), as_gregorian("2025-01-01"))
  expect_equal(as.Date(gregorian(2025, 4, 19)), as.Date("2025-04-19"))
  expect_equal(as_rd(gregorian(2025, 4, 19)), as_rd("2025-04-19"))
  expect_error(gregorian(2025, 13, 1))
  expect_error(gregorian(2025, 2, 30))
  expect_error(gregorian(2025, 2, 29))
  expect_error(gregorian(2024.4, 2, 12))
  expect_no_error(gregorian(2025, 2, 28))
  expect_no_error(gregorian(2020, 2, 29))
  expect_no_error(gregorian(2000, 2, 29))
  expect_error(gregorian(1900, 2, 29))
  expect_no_error(gregorian(NA, 2, 29))
  expect_error(gregorian(NA, 2, 30))
  expect_no_error(gregorian(NA, NA, 31))
  expect_error(gregorian(NA, NA, 32))
  expect_no_error(gregorian(NA, NA, NA))
  expect_no_error(as_gregorian(as_rd(NA_integer_)))
  expect_no_error(as_gregorian(as_rd(1:100)))
  expect_error(gregorian(1:3, 2:4, 5:6))
  expect_no_error(gregorian(1:3, 2:4, 5))

  expect_equal(
    tibble::tibble(x = rd_fixed(1:100), y = as_gregorian(x)) |> NROW(),
    100L
  )
})
