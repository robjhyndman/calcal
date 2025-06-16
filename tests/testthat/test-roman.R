test_that("roman", {
  expect_equal(roman_date(2025, 5, 1, 13, FALSE), as_roman("2025-05-02"))
  expect_error(roman_date(2025, 13, 1))
  expect_equal(as_roman("1990-02-21"), roman_date(1990, 2, 3, 6, FALSE))
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_roman() |>
      cal_roman$from_rd() |>
      cal_roman$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_no_error(as_roman(1:1e6) |> as.list() |> validate_roman())
})
