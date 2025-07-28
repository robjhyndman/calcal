test_that("babylonian", {
  expect_equal(
    babylonian_date(2152, 4, FALSE, 1:3) |>
      cal_babylonian$from_rd() |>
      cal_babylonian$to_rd(),
    babylonian_date(2152, 4, FALSE, 1:3) |> vctrs::vec_data()
  )
})
