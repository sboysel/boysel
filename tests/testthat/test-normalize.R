context("Normalize (rescale) vectors")

w <- 1:5
x <- c(w, NA)

test_that("normalize using base::scale behaves", {
  y <- normalize(w, fun = "scale")
  z <- normalize(x, fun = "scale")
  expect_equal(w[which(w == mean(w))], attr(y, "scaled:center"))
  expect_equal(sd(y), 1)
  expect_equal(length(x[is.na(x)]), length(z[is.na(z)]))
})

test_that("normalize using 'unit.range' behaves", {
  y <- normalize(w, fun = "unit.range")
  z <- normalize(x, fun = "unit.range", na.rm = TRUE)
  expect_true(all(max(y) == 1, min(y) == 0,
                  max(z, na.rm = TRUE) == 1, min(z, na.rm = TRUE) == 0))
  expect_equal(length(x[is.na(x)]), length(z[is.na(z)]))
})

test_that("normalize using 'unit.length' behaves", {
  y <- normalize(w, fun = "unit.length")
  expect_error(normalize(x, fun = "unit.length"))
})


test_that("normalize throws errors for bad arguments", {
  expect_error(normalize(letters))
  expect_error(normalize(1:5, "foo"))
})

test_that("normalize with custom functions behave", {
  f <- function(x, ...) (x - mean(x, ...)) / sd(x, ...)
  x <- 1:10
  xx <- normalize(x = x, fun = f, na.rm = TRUE)
  expect_equal(xx, as.numeric(scale(x)))
  f <- function(x) (x - mean(x)) / sd(x)
  expect_error(normalize(x = x, fun = f, na.rm = TRUE))
})
