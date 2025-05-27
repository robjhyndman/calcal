test_that("hebrew", {
  expect_equal(hebrew_date(5785, 4, 19), as_hebrew("2025-07-15"))
  expect_equal(
    as.Date(as_gregorian(hebrew_date(5785, 4, 19))),
    as.Date("2025-07-15")
  )
  expect_equal(as_rd(hebrew_date(5785, 4, 19)), as_rd("2025-07-15"))
  expect_error(hebrew_date(5785, 13, 1))
  expect_error(hebrew_date(2025, 2, 30))
  expect_error(hebrew_date(2024.4, 2, 12))
  expect_no_error(hebrew_date(2025, 2, 28))
  expect_no_error(as_hebrew(as_rd(1:100)))
  expect_error(hebrew_date(1:3, 2:4, 5:6))
  expect_no_error(hebrew_date(1:3, 2:4, 5))
  expect_error(hebrew_date(NA, 2, 30))
  expect_error(hebrew_date(NA, NA, 31))
  expect_error(hebrew_date(NA, NA, 32))
  # Holidays
  expect_equal(
    rosh_hashanah(2025:2030),
    as_gregorian(c(
      "2025-09-23",
      "2026-09-12",
      "2027-10-02",
      "2028-09-21",
      "2029-09-10",
      "2030-09-28"
    ))
  )
  expect_equal(
    passover(2025:2030),
    as_gregorian(c(
      "2025-04-13",
      "2026-04-02",
      "2027-04-22",
      "2028-04-11",
      "2029-03-31",
      "2030-04-18"
    ))
  )
  expect_equal(shavuot(2025:2030) - passover(2025:2030), rep(50, 6))
  expect_equal(
    yom_kippur(2025:2030),
    as_gregorian(c(
      "2025-10-02",
      "2026-09-21",
      "2027-10-11",
      "2028-09-30",
      "2029-09-19",
      "2030-10-07"
    ))
  )
  expect_equal(sukkot(2025:2030) - rosh_hashanah(2025:2030), rep(14, 6))
  expect_equal(
    hanukkah(2025:2030),
    as_gregorian(c(
      "2025-12-15",
      "2026-12-05",
      "2027-12-25",
      "2028-12-13",
      "2029-12-02",
      "2030-12-21"
    ))
  )
  expect_equal(
    purim(2025:2030),
    as_gregorian(c(
      "2025-03-14",
      "2026-03-03",
      "2027-03-23",
      "2028-03-12",
      "2029-03-01",
      "2030-03-19"
    ))
  )
})
