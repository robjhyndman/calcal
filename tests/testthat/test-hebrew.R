test_that("hebrew", {
  expect_equal(hebrew_date(5785, 4, 19), as_hebrew("2025-07-15"))
  expect_equal(
    as.Date(as_gregorian(hebrew_date(5785, 4, 19))),
    as.Date("2025-07-15")
  )
  expect_equal(as_rd(hebrew_date(5785, 4, 19)), as_rd("2025-07-15"))
  expect_error(hebrew_date(5785, 13, 1))
  expect_error(hebrew_date(2025, 2, 30))
  expect_error(hebrew_date(2024.4, 2, 12))
  expect_no_error(hebrew_date(2025, 2, 28))
  expect_no_error(as_hebrew(as_rd(1:100)))
  expect_error(hebrew_date(1:3, 2:4, 5:6))
  expect_no_error(hebrew_date(1:3, 2:4, 5))
  expect_error(hebrew_date(NA, 2, 30))
  expect_error(hebrew_date(NA, NA, 31))
  expect_error(hebrew_date(NA, NA, 32))
})
