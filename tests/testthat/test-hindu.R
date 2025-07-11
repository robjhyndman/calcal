test_that("hindu_solar", {
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_hindu_solar() |>
      cal_hindu_solar$from_rd() |>
      cal_hindu_solar$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_equal(
    hindu_solar_date(1947, 3, 1:10) |>
      cal_hindu_solar$from_rd() |>
      cal_hindu_solar$to_rd(),
    hindu_solar_date(1947, 3, 1:10) |> vctrs::vec_data()
  )
  expect_no_error(
    as_hindu_solar(1:1e5) |> as.list() |> validate_hindu_solar()
  )
})

test_that("hindu_lunar", {
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_hindu_lunar() |>
      cal_hindu_lunar$from_rd() |>
      cal_hindu_lunar$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_equal(
    hindu_lunar_date(1947, 3, FALSE, 1:10, FALSE) |>
      cal_hindu_lunar$from_rd() |>
      cal_hindu_lunar$to_rd(),
    hindu_lunar_date(1947, 3, FALSE, 1:10, FALSE) |> vctrs::vec_data()
  )
  expect_no_error(
    as_hindu_lunar(1:1e4) |> as.list() |> validate_hindu_lunar()
  )
})

test_that("old_hindu_solar", {
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_old_hindu_solar() |>
      cal_old_hindu_solar$from_rd() |>
      cal_old_hindu_solar$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_equal(
    old_hindu_solar_date(5126, 3, 1:10) |>
      cal_old_hindu_solar$from_rd() |>
      cal_old_hindu_solar$to_rd(),
    old_hindu_solar_date(5126, 3, 1:10) |> vctrs::vec_data()
  )
  expect_no_error(
    as_old_hindu_solar(1:1e5) |> as.list() |> validate_hindu_solar()
  )
})

test_that("old_hindu_lunar", {
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_old_hindu_lunar() |>
      cal_old_hindu_lunar$from_rd() |>
      cal_old_hindu_lunar$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_equal(
    old_hindu_lunar_date(5126, 3, FALSE, 1:10) |>
      cal_old_hindu_lunar$from_rd() |>
      cal_old_hindu_lunar$to_rd(),
    old_hindu_lunar_date(5126, 3, FALSE, 1:10) |> vctrs::vec_data()
  )
  expect_no_error(
    as_old_hindu_lunar(1:1e5) |> as.list() |> validate_hindu_lunar()
  )
})
