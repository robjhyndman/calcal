test_that("egyptian", {
  expect_equal(
    egyptian_date(2774, 3, 1:10) |>
      cal_egyptian$from_rd() |>
      cal_egyptian$to_rd(),
    egyptian_date(2774, 3, 1:10) |> vctrs::vec_data()
  )
})

test_that("armenian", {
  expect_equal(
    armenian_date(1474, 3, 1:10) |>
      cal_armenian$from_rd() |>
      cal_armenian$to_rd(),
    armenian_date(1474, 3, 1:10) |> vctrs::vec_data()
  )
})
