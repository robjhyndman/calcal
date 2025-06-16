test_that("old_hindu_solar", {
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_old_hindu_solar() |>
      cal_old_hindu_solar$from_rd() |>
      cal_old_hindu_solar$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_equal(
    old_hindu_solar_date(5126, 3, 1:10) |>
      cal_old_hindu_solar$from_rd() |>
      cal_old_hindu_solar$to_rd(),
    old_hindu_solar_date(5126, 3, 1:10) |> vctrs::vec_data()
  )
  expect_no_error(as_old_hindu_solar(1:1e6) |> as.list() |> validate_old_hindu_solar())
})
