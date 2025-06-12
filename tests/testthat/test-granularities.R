test_that("granularities", {
  date <- as.Date("2025-04-26") + 1:7
  expect_equal(granularity_names(as_iso(date)), c("year", "week", "day"))
  expect_equal(granularity_names(as_gregorian(date)), c("year", "month", "day"))
  expect_equal(
    granularity_names(as_roman(date)),
    c("year", "month", "event", "count", "leap")
  )
  expect_equal(granularity(as_iso(date), "week"), c(17, rep(18, 6)))
  expect_equal(
    granularity(as_gregorian(date), "month"),
    c(rep(4, 4), rep(5, 3))
  )
  expect_equal(granularity(as_roman(date), "count"), 18:12)
})
