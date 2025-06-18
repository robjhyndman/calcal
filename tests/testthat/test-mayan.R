test_that("mayan", {
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_mayan() |>
      cal_mayan$from_rd() |>
      cal_mayan$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_equal(
    mayan_date(13, 5, 6, 5, 0:6) |>
      cal_mayan$from_rd() |>
      cal_mayan$to_rd(),
    mayan_date(13, 5, 6, 5, 0:6) |> vctrs::vec_data()
  )
  expect_equal(
    gregorian_date(2012,12,21) |> as_mayan(),
    mayan_date(13, 0, 0, 0, 0)
  )
  expect_no_error(as_mayan(1:1e5) |> as.list() |> validate_mayan_long_count())
})
