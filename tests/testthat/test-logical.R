library(boysel)
context("Logical operators")

test_that("is_char_numeric()", {
  expect_identical(is_char_numeric(c(1, 3, "3", NA)), c(T, T, T, NA))
  expect_identical(is_char_numeric(c(1, 3, "3", NA, "A")), c(T, T, T, NA, F))
})

test_that("is_formula()", {
  expect_false(is_formula("y ~ x"))
  expect_true(is_formula(stats::as.formula("y ~ x")))
  expect_identical(is_formula(c(w ~ x, w ~ y, w ~ z)), c(T, T, T))
  expect_identical(is_formula(c(w ~ x, w ~ y, w ~ z, "y ~ x")), c(T, T, T, F))
})