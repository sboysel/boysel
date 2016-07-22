context("Summary Statistics")

test_that("cv() behaves", {
  x <- 1:5
  expect_identical(class(cv(x)), "numeric")
  expect_equal(length(cv(x)), 1)
  expect_equal(round(cv(x), 3), 0.527)
  x <- -5:5
  expect_identical(cv(x), Inf)
})

sumstats(mtcars)

test_that("", {
  ss <- sumstats(mtcars)
  expect_identical(dim(ss), c(11L, 9L))
  ss <- sumstats(iris, . ~ Species, digits = 3)
  expect_identical(dim(ss), c(12L, 10L))
  expect_true("Species" %in% names(ss))
  expect_identical(row.names(ss), as.character(1:nrow(ss)))
})