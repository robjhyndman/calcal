test_that("us_holidays", {
  expect_equal(us_independence_day(2025), gregorian_date(2025, 7, 4))
  expect_equal(us_labor_day(2025), gregorian_date(2025, 9, 1))
  expect_equal(us_memorial_day(2025), gregorian_date(2025, 5, 26))
  expect_equal(us_election_day(2025), gregorian_date(2025, 11, 4))
  expect_equal(us_daylight_saving_start(2025), gregorian_date(2025, 3, 9))
  expect_equal(us_daylight_saving_end(2025), gregorian_date(2025, 11, 2))
})
