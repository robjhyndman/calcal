test_that("astronomical", {
  melbourne <- location(-37.8136, 144.9631, 31, 10)
  expect_equal(
    sunrise(as_rd("2025-01-01"), melbourne),
    time_of_day(hour = 5, minute = 1, second = 0.723),
    tolerance = 1e-3
  )
  expect_equal(
    sunset(as_rd("2025-01-01"), melbourne),
    time_of_day(hour = 19, minute = 55, second = 8.64),
    tolerance = 1e-3
  )
})
