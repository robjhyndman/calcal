test_that("roman", {
  expect_equal(roman_date(2025, 5, 1, 13, FALSE), as_roman("2025-05-02"))
  expect_error(roman_date(2025, 13, 1))
  expect_equal(as_roman("1990-02-21"), roman_date(1990, 2, 3, 6, FALSE))
})
