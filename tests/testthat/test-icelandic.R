test_that("icelandic", {
  expect_equal(
    icelandic_date(2025, 1, 6, 0:6) |>
      cal_icelandic$from_rd() |>
      cal_icelandic$to_rd(),
    icelandic_date(2025, 1, 6, 0:6) |> vctrs::vec_data()
  )
})
