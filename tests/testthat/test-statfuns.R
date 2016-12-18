context("Summary Statistics")

test_that("cv() behaves", {
  x <- 1:5
  expect_is(cv(x), "numeric")
  expect_equal(length(cv(x)), 1)
  expect_equal(round(cv(x), 3), 0.527)
  x <- -5:5
  expect_identical(cv(x), Inf)
})

test_that("most_freq() behaves", {
  x <- c(0, 1, 2, 3, 4, 5, 0)
  y <- c("foo", "bar", "baz", "foo")
  z <- factor(y)
  expect_is(most_freq(x), "numeric")
  expect_is(most_freq(y), "character")
  expect_is(most_freq(z), "factor")
  expect_equal(most_freq(x), 0)
  expect_equal(most_freq(y), "foo")
  expect_equal(most_freq(z), factor("foo"))
  xx <- c(x, 1)
  yy <- c(y, "bar")
  zz <- factor(yy)
  expect_is(most_freq(xx), "numeric")
  expect_is(most_freq(yy), "character")
  expect_is(most_freq(zz), "factor")
  expect_equal(most_freq(xx), c(0, 1))
  expect_equal(most_freq(yy), c("bar", "foo"))
  expect_equal(most_freq(zz), factor(c("bar", "foo")))
})