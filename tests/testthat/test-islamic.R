test_that("islamic", {
  expect_equal(islamic_date(1446, 7, 1), as_islamic("2025-01-01"))
  expect_equal(
    as.Date(as_gregorian(islamic_date(1446, 7, 1))),
    as.Date("2025-01-01")
  )
  expect_equal(
    vec_data(islamic_date(1446, 7, 1)),
    vec_data(gregorian_date(2025, 1, 1))
  )
  expect_error(islamic_date(1446, 13, 1))
  expect_error(islamic_date(2025, 2, 31))
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_islamic() |>
      cal_islamic$from_rd() |>
      cal_islamic$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_no_error(as_islamic(1:1e5) |> as.list() |> validate_islamic())

  # Holidays
  expect_equal(
    islamic_new_year(2025:2026),
    as_gregorian(c("2025-06-27", "2026-06-17"))
  )
  expect_equal(
    mawlid(2025:2026),
    as_gregorian(c("2025-08-07", "2026-07-28"))
  )
  expect_equal(
    ramadan(2025:2026),
    as_gregorian(c("2025-03-01", "2026-02-18"))
  )
  expect_equal(
    eid_al_fitr(2025:2026),
    as_gregorian(c("2025-03-31", "2026-03-20"))
  )
  expect_equal(
    eid_al_adha(2025:2026),
    as_gregorian(c("2025-06-07", "2026-05-27"))
  )
})


test_that("oislamic", {
  expect_equal(oislamic_date(1446, 6, 30), as_oislamic("2025-01-01"))
  expect_equal(
    as.Date(as_gregorian(oislamic_date(1446, 6, 30))),
    as.Date("2025-01-01")
  )
  expect_equal(
    vec_data(oislamic_date(1446, 6, 30)),
    vec_data(gregorian_date(2025, 1, 1))
  )
  expect_error(oislamic_date(1446, 13, 1))
  expect_error(oislamic_date(2025, 2, 31))
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_oislamic() |>
      cal_oislamic$from_rd() |>
      cal_oislamic$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_no_error(as_oislamic(1:1e3) |> as.list() |> validate_islamic())
})

test_that("saudi", {
  expect_equal(saudi_date(1446, 6, 30), as_saudi("2025-01-01"))
  expect_equal(
    as.Date(as_gregorian(saudi_date(1446, 6, 30))),
    as.Date("2025-01-01")
  )
  expect_equal(
    vec_data(saudi_date(1446, 6, 30)),
    vec_data(gregorian_date(2025, 1, 1))
  )
  expect_error(saudi_date(1446, 13, 1))
  expect_error(saudi_date(2025, 2, 31))
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_saudi() |>
      cal_saudi$from_rd() |>
      cal_saudi$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
})
