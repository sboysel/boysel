context("Recode")

test_that("recode returns appropriate object types", {
  x <- c(1:5, NA)
  r <- recode(x, `2` = c(3, 5), `1` = c(NA, 1), .na = NULL, .default = 3)
  expect_identical(r, c("1", "3", "2", "3", "2", "1"))
})