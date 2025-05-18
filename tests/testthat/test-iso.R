test_that("iso", {
  dec25 <- as_iso(gregorian(2025,12,1:31))
  expect_equal(week_of_year(dec25), vctrs::field(dec25, "week"))
})
