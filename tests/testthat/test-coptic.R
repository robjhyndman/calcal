test_that("coptic", {
  expect_equal(
    coptic_date(2774, 3, 1:10) |>
      cal_coptic$from_rd() |>
      cal_coptic$to_rd(),
    coptic_date(2774, 3, 1:10) |> vctrs::vec_data()
  )
})

test_that("ethiopic", {
  expect_equal(
    ethiopic_date(1474, 3, 1:10) |>
      cal_ethiopic$from_rd() |>
      cal_ethiopic$to_rd(),
    ethiopic_date(1474, 3, 1:10) |> vctrs::vec_data()
  )
})
