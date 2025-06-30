test_that("coptic", {
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_coptic() |>
      cal_coptic$from_rd() |>
      cal_coptic$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_equal(
    coptic_date(2774, 3, 1:10) |>
      cal_coptic$from_rd() |>
      cal_coptic$to_rd(),
    coptic_date(2774, 3, 1:10) |> vctrs::vec_data()
  )
  expect_no_error(
    as_coptic(1:1e5) |> as.list() |> validate_coptic()
  )
})

test_that("ethiopic", {
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_ethiopic() |>
      cal_ethiopic$from_rd() |>
      cal_ethiopic$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_equal(
    ethiopic_date(1474, 3, 1:10) |>
      cal_ethiopic$from_rd() |>
      cal_ethiopic$to_rd(),
    ethiopic_date(1474, 3, 1:10) |> vctrs::vec_data()
  )
  expect_no_error(
    as_ethiopic(1:1e5) |> as.list() |> validate_coptic()
  )
})
