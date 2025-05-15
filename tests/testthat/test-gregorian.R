test_that("gregorian", {
  expect_equal(gregorian(2025, 1, 1), as_gregorian("2025-01-01"))
  expect_equal(as.Date(gregorian(2025, 4, 19)), as.Date("2025-04-19"))
  expect_equal(as_rd(gregorian(2025, 4, 19)), as_rd("2025-04-19"))
  expect_error(gregorian(2025, 13, 1))
  expect_error(gregorian(2025, 2, 30))
  expect_error(gregorian(2025, 2, 29))
  expect_error(gregorian(2024.4, 2, 12))
  expect_no_error(gregorian(2025, 2, 28))
  expect_no_error(gregorian(2020, 2, 29))
  expect_no_error(gregorian(2000, 2, 29))
  expect_error(gregorian(1900, 2, 29))
  expect_no_error(gregorian(NA, 2, 29))
  expect_error(gregorian(NA, 2, 30))
  expect_no_error(gregorian(NA, NA, 31))
  expect_error(gregorian(NA, NA, 32))
  expect_no_error(gregorian(NA, NA, NA))
  expect_no_error(as_gregorian(as_rd(NA_integer_)))
  expect_no_error(as_gregorian(as_rd(1:100)))
  expect_error(gregorian(1:3, 2:4, 5:6))
  expect_no_error(gregorian(1:3, 2:4, 5))

  expect_equal(
    tibble::tibble(x = rd_fixed(1:100), y = as_gregorian(x)) |> NROW(),
    100L
  )
})

test_that("parts_of_date", {
  yr2025 <- gregorian(2025,1,1) + (0:364)
  expect_equal(day_of_week(yr2025)[1], "Wednesday")
  expect_equal(day_of_month(yr2025), c(1:31,1:28,1:31,1:30,1:31,1:30,1:31,1:31,1:30,1:31,1:30,1:31))
  expect_equal(day_of_year(yr2025), 1:365)
  expect_equal(days_remaining(yr2025), 364:0)
  expect_equal(month_of_year(yr2025), c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31)))
  expect_equal(week_of_year(yr2025), c(rep(1,5), rep(2:52, rep(7,51)), rep(1, 3)))
})
