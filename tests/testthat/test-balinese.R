test_that("balinese", {
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_balinese() |>
      cal_balinese$from_rd() |>
      cal_balinese$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_equal(
    balinese_date(FALSE,1,1:2,1:2,3:4,1:2,1:2,5:6,7:8,3:4) |>
      cal_balinese$from_rd() |>
      cal_balinese$to_rd() |>
      suppressWarnings(),
    gregorian_date(2025, 6,14:15) |> vctrs::vec_data()
  )
})
