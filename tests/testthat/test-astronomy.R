test_that("astronomical", {
  melbourne <- location(-37.8136, 144.9631, 31, 10)
  expect_equal(
    sunrise(as_rd("2025-01-01"), melbourne),
    time_of_day(hour = 5, minute = 0, second = 25.00),
    tolerance = 1e-3
  )
  expect_equal(
    sunset(as_rd("2025-01-01"), melbourne),
    time_of_day(hour = 19, minute = 46, second = 39.75),
    tolerance = 1e-3
  )
})

test_that("lunar", {
  april2025 <- seq(as.Date("2025-04-01"), as.Date("2025-04-30"), by = "1 day")
  expect_true(all(diff(lunar_phase(april2025)) < 15))
  # New moon
  expect_true(abs(lunar_phase("2025-04-28")) < 3)
  expect_true(as_gregorian("2025-04-28") %in% new_moons(2025))
  # Full moon
  expect_true(abs(lunar_phase("2025-04-13") - 180) < 1)
  expect_true(as_gregorian("2025-04-13") %in% full_moons(2025))
})
