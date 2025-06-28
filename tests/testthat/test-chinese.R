test_that("chinese", {
  expect_equal(
    dragon_festival(2025:2026),
    gregorian_date(2025:2026, c(5, 6), c(31, 19))
  )
  expect_equal(
    chinese_new_year(2025:2026),
    gregorian_date(2025:2026, c(1, 2), c(29, 17))
  )
  expect_equal(
    qing_ming(2025:2026),
    gregorian_date(2025:2026, c(4, 4), c(4, 5))
  )
  expect_equal(
    as_gregorian(Sys.Date()),
    as_chinese(Sys.Date()) |> as_gregorian()
  )
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_chinese() |>
      cal_chinese$from_rd() |>
      cal_chinese$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_equal(
    gregorian_date(2025, 6, 1:10) |>
      as_chinese() |>
      cal_chinese$from_rd() |>
      cal_chinese$to_rd(),
    gregorian_date(2025, 6, 1:10) |> vctrs::vec_data()
  )
  expect_no_error(as_chinese(1:1e4) |> as.list() |> validate_chinese())
})

test_that("japanese", {
  expect_equal(
    as_gregorian(Sys.Date()),
    as_japanese(Sys.Date()) |> as_gregorian()
  )
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_japanese() |>
      cal_japanese$from_rd() |>
      cal_japanese$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_equal(
    gregorian_date(2025, 6, 1:10) |>
      as_japanese() |>
      cal_japanese$from_rd() |>
      cal_japanese$to_rd(),
    gregorian_date(2025, 6, 1:10) |> vctrs::vec_data()
  )
  expect_no_error(as_japanese(1:1e4) |> as.list() |> validate_chinese())
})

test_that("korean", {
  expect_equal(
    as_gregorian(Sys.Date()),
    as_korean(Sys.Date()) |> as_gregorian()
  )
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_korean() |>
      cal_korean$from_rd() |>
      cal_korean$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_equal(
    gregorian_date(2025, 6, 1:10) |>
      as_korean() |>
      cal_korean$from_rd() |>
      cal_korean$to_rd(),
    gregorian_date(2025, 6, 1:10) |> vctrs::vec_data()
  )
  expect_no_error(as_korean(1:1e4) |> as.list() |> validate_korean())
})

test_that("vietnamese", {
  expect_equal(
    as_gregorian(Sys.Date()),
    as_vietnamese(Sys.Date()) |> as_gregorian()
  )
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_vietnamese() |>
      cal_vietnamese$from_rd() |>
      cal_vietnamese$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_equal(
    gregorian_date(2025, 6, 1:10) |>
      as_vietnamese() |>
      cal_vietnamese$from_rd() |>
      cal_vietnamese$to_rd(),
    gregorian_date(2025, 6, 1:10) |> vctrs::vec_data()
  )
  expect_no_error(as_vietnamese(1:1e4) |> as.list() |> validate_chinese())
})
