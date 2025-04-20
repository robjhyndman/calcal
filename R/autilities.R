#' @importFrom S7 class_any class_numeric class_character class_logical class_Date
#' @importFrom S7 new_class new_generic method `method<-`

adjusted_mod <- function(x, y) {
  # The value of (x mod y) with y instead of 0
  y + (x %% y)
}

# Arguments past as a list to these functions

all_equal_length <- function(args) {
  # Check if all arguments are of equal length
  lengths <- unlist(lapply(args, length))
  all(lengths == lengths[1])
}

all_numeric <- function(args) {
  # Check if all arguments are numeric
  all(unlist(lapply(args, is.numeric)))
}

all_integer <- function(args) {
  # Check if all arguments are integer
  all(unlist(lapply(args, function(x) all(abs(x - round(x)) < 1e-10))))
}


# Print date
print_date <- function(suffix, parts) {
  print(
    paste(
      suffix,
      apply(as.data.frame(parts), 1, function(x) {
        paste(sprintf("%.2d", x), collapse = "-")
      }),
      sep = ""
    )
  )
}
