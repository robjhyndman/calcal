test_that("tibetan", {
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_tibetan() |>
      cal_tibetan$from_rd() |>
      cal_tibetan$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_equal(
    tibetan_date(2152, 4, FALSE, 1:30, FALSE) |>
      cal_tibetan$from_rd() |>
      cal_tibetan$to_rd(),
    tibetan_date(2152, 4, FALSE, 1:30, FALSE) |> vctrs::vec_data()
  )
  # Validation
  d <- as_tibetan(1:1e5) |> as.list() |> validate_tibetan()
  # New year
  expect_equal(
    tibetan_new_year(2025:2026),
    gregorian_date(2025:2026, 2, c(28, 18))
  )
})
