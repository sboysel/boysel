context("Summary Statistics")

test_that("sumstats variable permutations output correct number of dimensions", {
  ss <- sumstats(mtcars)
  expect_is(ss, "data.frame")
  expect_identical(dim(ss), c(11L, 9L))
  ss <- sumstats(iris, . ~ Species, digits = 3)
  expect_is(ss, "data.frame")
  expect_identical(dim(ss), c(12L, 10L))
  expect_true("Species" %in% names(ss))
  expect_identical(row.names(ss), as.character(1:nrow(ss)))
})

test_that("sumstats works on numeric vectors", {
 ss <- sumstats(mtcars$disp)
 expect_true(nrow(ss) == 1)
 expect_is(ss, "data.frame")
})

test_that("n is converted to an integer column", {
  ss <- sumstats(mtcars$disp)
  expect_is(ss$n, "integer")
})

test_that("sumstats_row takes a numeric vector and returns a numeric vector of length 8", {
  x <- c(runif(5), NA, NaN, Inf, -Inf)
  xx <- sumstats_row(x)
  expect_identical(names(xx), c("n", "mean", "sd", "min", "p25", "p50", "p75", "max"))
  expect_is(xx, "numeric")
  expect_true(length(xx) == 8)
})