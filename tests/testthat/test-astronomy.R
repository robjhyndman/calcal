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
  # Urbana sunset (p422)
  expect_equal(
    sunset(gregorian_date(1945, 11, 12), location(40.1, -88.2, 225, -6)),
    time_of_day(16, 42, 10.74),
    tolerance = .001
  )
  # Nunavut during summer
  expect_equal(
    sunset(gregorian_date(1945, 11, 12), location(82.3, -62.19, 0, -5)),
    time_of_day(NA, NA, NA)
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
  melbourne <- location(-37.8136, 144.9631, 31, 10)
  expect_equal(
    moonrise(gregorian_date(2025, 1, 1), melbourne),
    time_of_day(hour = 6, minute = 43, second = 0),
    tolerance = .1
  )
  expect_equal(
    moonset(gregorian_date(2025, 1, 1), melbourne),
    time_of_day(hour = 23, minute = 10, second = 18),
    tolerance = .1
  )
})
