test_that("astronomical", {
  melbourne <- location(-37.8136, 144.9631, 31, 10)
  expect_equal(
    sunrise(as_rd("2025-01-01"), melbourne),
    time_of_day(hour = 5, minute = 0, second = 21.50),
    tolerance = 1e-3
  )
  expect_equal(
    sunset(as_rd("2025-01-01"), melbourne),
    time_of_day(hour = 19, minute = 54, second = 50.39),
    tolerance = 1e-3
  )
})
