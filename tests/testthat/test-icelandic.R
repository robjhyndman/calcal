test_that("icelandic", {
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_icelandic() |>
      cal_icelandic$from_rd() |>
      cal_icelandic$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_equal(
    icelandic_date(2025, 1, 6, 0:6) |>
      cal_icelandic$from_rd() |>
      cal_icelandic$to_rd(),
    icelandic_date(2025, 1, 6, 0:6) |> vctrs::vec_data()
  )
  expect_no_error(as_icelandic(1:1e6) |> as.list() |> validate_icelandic())
})
