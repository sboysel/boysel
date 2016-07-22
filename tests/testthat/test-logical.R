context("Logical operators")

test_that("is_char_numeric()", {
  x <- c(1, 3, "3", NA)
  xx <- c(x, "A")
  expect_true(is_char_numeric(x))
  expect_false(is_char_numeric(xx))
})