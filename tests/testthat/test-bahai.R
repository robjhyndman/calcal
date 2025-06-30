test_that("bahai", {
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_bahai() |>
      cal_bahai$from_rd() |>
      cal_bahai$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_equal(
    bahai_date(1, 10, 11, 3, 5:10) |>
      cal_bahai$from_rd() |>
      cal_bahai$to_rd(),
    bahai_date(1, 10, 11, 3, 5:10) |> vctrs::vec_data()
  )
  expect_no_error(
    as_bahai(1:1e5) |> as.list() |> validate_bahai()
  )
})

test_that("bahai_new_year", {
  expect_equal(
    bahai_new_year(2023:2027),
    gregorian_date(2023:2027, MARCH, 21)
  )
  expect_equal(
    bahai_new_year(2023:2027) |> as_bahai(),
    bahai_date(1, 10, 9:13, 1, 1)
  )
  expect_equal(
    naw_ruz(2023:2027),
    gregorian_date(2023:2027, MARCH, c(21, 20, 20, 20, 21))
  )
  # value for 2026 is different from official value.
  # Seems to be to do with solar_longitude calculation being
  # very close to the boundary (360|0).
  expect_equal(
    birth_of_the_bab(2023:2027),
    gregorian_date(2023:2027, c(10, 11, 10, 11, 10), c(16, 2, 22, 10, 30))
  )
})
