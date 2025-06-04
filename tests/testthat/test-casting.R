test_that("casting", {
  today <- as_rd(Sys.Date()) |> as.integer()
  expect_equal(
    today,
    as_gregorian(Sys.Date()) |> as.integer()
  )
  expect_equal(
    today,
    as_hebrew(Sys.Date()) |> as.integer()
  )
  expect_equal(
    today,
    as_islamic(Sys.Date()) |> as.integer()
  )
  expect_equal(
    today,
    as_iso(Sys.Date()) |> as.integer()
  )
  expect_equal(
    today,
    as_julian(Sys.Date()) |> as.integer()
  )
  expect_equal(
    today,
    as_roman(Sys.Date()) |> as.integer()
  )
})
