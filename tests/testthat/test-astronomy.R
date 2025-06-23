test_that("astronomical", {
  melbourne <- location(-37.8136, 144.9631, 31, 10)
  expect_equal(
    sunrise(gregorian_date(2025, 1, 1), melbourne),
    time_of_day(hour = 5, minute = 0, second = 25.00),
    tolerance = 1e-3
  )
  expect_equal(
    sunset(gregorian_date(2025, 1, 1), melbourne),
    time_of_day(hour = 19, minute = 46, second = 39.75),
    tolerance = 1e-3
  )
})

test_that("lunar", {
  april2025 <- gregorian_date(2025, 4, 1:30)
  expect_true(all(diff(lunar_phase(april2025)) < 15))
  # New moon
  expect_true(abs(lunar_phase(as_gregorian("2025-04-28"))) < 3)
  expect_true(as_gregorian("2025-04-27") %in% new_moons(2025))
  # Full moon
  expect_true(abs(lunar_phase(as_gregorian("2025-04-13")) - 180) < 1)
  expect_true(as_gregorian("2025-04-13") %in% full_moons(2025))

  expect_equal(
    moonrise(gregorian_date(2025,1,1), melbourne),
    time_of_day(hour = 6, minute = 43, second = 0),
    tolerance = 1
  )
  expect_equal(
    moonset(gregorian_date(2025,1,1), melbourne),
    time_of_day(hour = 22, minute = 09, second = 0),
    tolerance = 1
  )
})
