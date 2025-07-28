test_that("missing", {
  calcal_env <- as.environment("package:calcal")
  funs <- Filter(\(x) grepl("^cal_", x), ls(calcal_env))
  mget(funs, envir = calcal_env) |>
    lapply(\(x) {
      expect_equal(c(NA, 10), x$from_rd(c(NA, 10)) |> x$to_rd())
    })
})
