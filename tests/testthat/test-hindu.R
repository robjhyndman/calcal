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
  expect_equal(
    as_hindu_solar(0),
    hindu_solar_date(-78, 10, 20)
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
  expect_equal(
    as_hindu_lunar(0),
    hindu_lunar_date(57, 10, FALSE, 19, FALSE)
  )
  expect_no_error(
    as_hindu_lunar(1:1e4) |> as.list() |> validate_hindu_lunar()
  )
  expect_equal(
    vec_data(gregorian_date(2025, 7, 15)),
    vec_data(hindu_lunar_date(2082, 4, FALSE, 20, FALSE))
  )
})

test_that("hindu_holidays", {
  expect_true(
    # Get within 2 days for Diwali
    all(
      abs(
        diwali(2024:2026) -
          gregorian_date(2024:2026, c(11, 10, 11), c(1, 20, 8))
      ) <=
        2
    )
  )
  expect_true(
    # Get within 1 day for Shiva
    all(
      abs(
        shiva(2024:2026) -
          gregorian_date(2024:2026, c(3, 2, 2), c(9, 26, 16))
      ) <=
        1
    )
  )
  expect_true(
    # Get within 1 day of Rama Navami
    all(
      abs(
        rama(2024:2026) -
          gregorian_date(2024:2026, c(4, 4, 3), c(17, 6, 27))
      ) <=
        1
    )
  )
  expect_equal(
    mesha_sankranti(c(2000, 2025:2026)),
    gregorian_date(c(2000, 2025:2026), 4, c(13, 14, 14))
  )
  expect_equal(
    sacred_wednesdays(2025:2026),
    as_gregorian(c("2025-2-5", "2025-10-29", "2026-7-22"))
  )
  expect_equal(
    hindu_lunar_new_year(2024:2026),
    gregorian_date(2024:2026, c(4, 3, 3), c(9, 30, 19))
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
