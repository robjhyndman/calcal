test_that("roman", {
  expect_equal(roman_date(2025, 5, 1, 13, FALSE), as_roman("2025-05-02"))
  expect_error(roman_date(2025, 13, 1))
})
