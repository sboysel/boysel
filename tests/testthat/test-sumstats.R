context("Summary Statistics")

sumstats(mtcars)

test_that("sumstats variable permutations output correct number of dimensions", {
  ss <- sumstats(mtcars)
  expect_identical(dim(ss), c(11L, 9L))
  ss <- sumstats(iris, . ~ Species, digits = 3)
  expect_identical(dim(ss), c(12L, 10L))
  expect_true("Species" %in% names(ss))
  expect_identical(row.names(ss), as.character(1:nrow(ss)))
})