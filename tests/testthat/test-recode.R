library(boysel)
context("Recode")

x <- c(1:5, NA)

test_that("recode()", {
  r <- recode(x, `2` = c(3, 5), `1` = c(NA, 1), .na = NULL, .default = 3)
  rr <- recode(x, `2` = 1, `1` = 2, .na = NA, factor = TRUE)
  expect_identical(x, recode(x))
  expect_identical(r, c("1", "3", "2", "3", "2", "1"))
  expect_identical(boysel::as_numeric(rr), c(2, 1, 3, 4, 5, NA))
  expect_is(rr, "factor")
  expect_message(recode(x), "No default value set")
})

test_that("binary_recode()", {
  r <- binary_recode(x, c(1, 2))
  expect_is(r, "integer")
  expect_identical(r, as.integer(c(1, 1, 0, 0, 0, NA)))
})