test_that("french", {
  expect_equal(
    french_date(233, 8, 1:10) |>
      cal_french$from_rd() |>
      cal_french$to_rd(),
    french_date(233, 8, 1:10) |> vctrs::vec_data()
  )
})

test_that("afrench", {
  expect_equal(
    afrench_date(1474, 3, 1:10) |>
      cal_afrench$from_rd() |>
      cal_afrench$to_rd(),
    afrench_date(1474, 3, 1:10) |> vctrs::vec_data()
  )
})
