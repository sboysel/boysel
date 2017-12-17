context("Logical operators")

test_that("is_char_numeric()", {
  x <- c(1, 3, "3", NA)
  y <- c(x, "A")
  expect_equal(is_char_numeric(x), c(T, T, T, T))
  expect_equal(is_char_numeric(y), c(T, T, T, T, F))
})

test_that("is_formula()", {
  w <- "y ~ x"
  x <- stats::as.formula(w)
  y <- c(w ~ x, w ~ y, w ~ z)
  z <- c(y, w)
  expect_false(is_formula(w))
  expect_true(is_formula(x))
  expect_equal(is_formula(y), c(T, T, T))
  expect_equal(is_formula(z), c(T, T, T, F))
})