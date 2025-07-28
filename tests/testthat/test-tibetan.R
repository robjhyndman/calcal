test_that("tibetan", {
  expect_equal(
    tibetan_date(2152, 4, FALSE, 1:30, FALSE) |>
      cal_tibetan$from_rd() |>
      cal_tibetan$to_rd(),
    tibetan_date(2152, 4, FALSE, 1:30, FALSE) |> vctrs::vec_data()
  )
  # New year
  expect_equal(
    tibetan_new_year(2025:2026),
    gregorian_date(2025:2026, 2, c(28, 18))
  )
})
