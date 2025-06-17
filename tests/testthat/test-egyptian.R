test_that("egyptian", {
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_egyptian() |>
      cal_egyptian$from_rd() |>
      cal_egyptian$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_equal(
    egyptian_date(2774, 3, 1:10) |>
      cal_egyptian$from_rd() |>
      cal_egyptian$to_rd(),
    egyptian_date(2774, 3, 1:10) |> vctrs::vec_data()
  )
  expect_no_error(
    as_egyptian(1:1e5) |> as.list() |> validate_egyptian()
  )
})

test_that("armenian", {
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_armenian() |>
      cal_armenian$from_rd() |>
      cal_armenian$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_equal(
    armenian_date(1474, 3, 1:10) |>
      cal_armenian$from_rd() |>
      cal_armenian$to_rd(),
    armenian_date(1474, 3, 1:10) |> vctrs::vec_data()
  )
  expect_no_error(
    as_armenian(1:1e5) |> as.list() |> validate_egyptian()
  )
})
