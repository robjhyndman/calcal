test_that("balinese", {
  expect_equal(
    gregorian_date(1967, 5, 2) |>
      as_balinese() |>
      cal_balinese$from_rd() |>
      cal_balinese$to_rd(),
    gregorian_date(1967, 5, 2) |> as.numeric()
  )
  expect_equal(
    balinese_date(FALSE, 1, 1:2, 1:2, 3:4, 1:2, 1:2, 5:6, 7:8, 3:4) |>
      cal_balinese$from_rd() |>
      cal_balinese$to_rd() |>
      suppressWarnings(),
    gregorian_date(2025, 6, 15:16) |> vctrs::vec_data()
  )
  expect_no_error(as_balinese(1:1e5) |> as.list() |> validate_balinese())
  # Tumpek
  tumpek2025 <- tumpek(2025)
  expect_equal(diff(tumpek2025), rep(35, 9))
  expect_equal(tumpek2025[1], gregorian_date(2025, 1, 18))
  # Kajeng Keliwon
  kk2025 <- kajeng_keliwon(2025)
  expect_equal(diff(kk2025), rep(15, 24))
  expect_equal(kk2025[1], gregorian_date(2025, 1, 3))
})
