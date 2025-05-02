test_that("day_number", {
  days <- as_gregorian(seq(as.Date("2025-01-01"), by = "day", length = 365))
  expect_true(all(day_number(days) + days_remaining(days) == 365))
})
