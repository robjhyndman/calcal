test_that("persian", {
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_persian() |>
      cal_persian$from_rd() |>
      cal_persian$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_equal(
    persian_date(233, 8, 1:10) |>
      cal_persian$from_rd() |>
      cal_persian$to_rd(),
    persian_date(233, 8, 1:10) |> vctrs::vec_data()
  )
  expect_no_error(
    as_persian(1:1e5) |> as.list() |> validate_persian()
  )
  expect_equal(
    persian_date(1404, 1, 1) |> vec_data(),
    gregorian_date(2025, 3, 21) |> vec_data()
  )
  expect_equal(
    apersian_date(1404, 1, 1) |> vec_data(),
    gregorian_date(2025, 3, 20) |> vec_data()
  )
  expect_equal(
    apersian_date(1404, 1, 1) |>
      cal_apersian$from_rd() |>
      cal_apersian$to_rd(),
    739330
  )
})
