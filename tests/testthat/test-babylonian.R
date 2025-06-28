test_that("babylonian", {
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_babylonian() |>
      cal_babylonian$from_rd() |>
      cal_babylonian$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_equal(
    babylonian_date(2152, 4, FALSE, 1:5) |>
      cal_babylonian$from_rd() |>
      cal_babylonian$to_rd(),
    babylonian_date(2152, 4, FALSE, 1:5) |> vctrs::vec_data()
  )
})
