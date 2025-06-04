test_that("granularities", {
  expect_equal(granularities(iso_date(2025, 23, 2:4)), c("year", "week", "day"))
  expect_equal(
    granularities(gregorian_date(2025, 4, 19:30)),
    c("year", "month", "day")
  )
  expect_equal(
    granularities(as_roman(Sys.Date())),
    c("year", "month", "event", "count", "leap")
  )
  expect_equal(granularity(iso_date(2025, 23, 2:4), "week"), 23)
  expect_equal(granularity(gregorian_date(2025, 4, 19:21), "month"), 4)
  expect_equal(granularity(as_roman(Sys.Date()), "event"), 1)
})
