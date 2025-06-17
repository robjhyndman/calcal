test_that("french", {
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_french() |>
      cal_french$from_rd() |>
      cal_french$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_equal(
    french_date(233, 8, 1:10) |>
      cal_french$from_rd() |>
      cal_french$to_rd(),
    french_date(233, 8, 1:10) |> vctrs::vec_data()
  )
  expect_no_error(
    as_french(1:1e5) |> as.list() |> validate_french()
  )
})

test_that("afrench", {
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_afrench() |>
      cal_afrench$from_rd() |>
      cal_afrench$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_equal(
    afrench_date(1474, 3, 1:10) |>
      cal_afrench$from_rd() |>
      cal_afrench$to_rd(),
    afrench_date(1474, 3, 1:10) |> vctrs::vec_data()
  )
  expect_no_error(
    as_afrench(1:1e5) |> as.list() |> validate_french()
  )
})
