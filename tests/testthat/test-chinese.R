test_that("multiplication works", {
  expect_equal(dragon_festival(2025:2026),
    gregorian_date(2025:2026, c(5,6), c(31,19)))
  expect_equal(chinese_new_year(2025:2026),
    gregorian_date(2025:2026, c(1,2), c(29,17)))
  expect_equal(qing_ming(2025:2026),
    gregorian_date(2025:2026, c(4,4), c(4,4)))
})
