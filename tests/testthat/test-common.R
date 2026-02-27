calcal_env <- as.environment("package:calcal")
funs <- Filter(\(x) grepl("^cal_", x), ls(calcal_env))
calendars <- mget(funs, envir = calcal_env)

funs <- Filter(\(x) grepl("^as_", x), ls(calcal_env))
as_funs <- Filter(\(x) !grepl("^as_date", x), funs)
as_funs <- Filter(\(x) !grepl("^as_time", x), as_funs)
as_funs <- Filter(\(x) !grepl("^as_list", x), as_funs)
as_funs <- mget(as_funs, envir = calcal_env)


test_that("missing", {
  calendars |>
    lapply(\(x) {
      #print(x$name)
      expect_equal(c(NA, 10), x$from_rd(c(NA, 10)) |> x$to_rd())
    })
})

test_that("birthday", {
  calendars |>
    lapply(\(x) {
      #print(x$name)
      expect_equal(
        gregorian_date(1967, 5, 2) |>
          x$from_rd() |>
          x$to_rd(),
        gregorian_date(1967, 5, 2) |> as.numeric()
      )
    })
})

test_that("today", {
  today <- as_gregorian("2026-02-25") |> as.integer()
  calendars |>
    lapply(\(x) {
      today_x <- as_date(today, calendar = x) |> as.integer()
      expect_equal(today, today_x)
      expect_equal(
        today + c(0:3, NA),
        (today + c(0:3, NA)) |>
          x$from_rd() |>
          x$to_rd()
      )
    })
})

test_that("validation", {
  # Drop slow calendars
  vslow <- paste0(
    "cal_",
    c(
      "babylonian",
      "ohebrew",
      "oislamic",
      "samaritan",
      "saudi"
    )
  )
  slow <- paste0(
    "cal_",
    c(
      "chinese",
      "japanese",
      "korean",
      "vietnamese",
      "hebrew",
      "hindu_lunar"
    )
  )
  calendars[names(calendars) %in% slow] |>
    lapply(\(x) {
      #print(x$name)
      d <- x$from_rd(1:1e4) |>
        as.list() |>
        x$validate()
      expect_no_error(d)
    })
  calendars[!(names(calendars) %in% c(vslow, slow))] |>
    lapply(\(x) {
      #print(x$name)
      d <- x$from_rd(1:1e5) |>
        as.list() |>
        x$validate()
      expect_no_error(d)
    })
})
