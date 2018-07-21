library(boysel)
context("Factors")

test_that("Convert factor vectors to actual numerical values", {
  x <- c(0.1, 1.1, 2.1, -3.4)
  f <- factor(x)
  expect_identical(x, as_numeric(f))
  expect_identical(x, as_numeric(x))
})
