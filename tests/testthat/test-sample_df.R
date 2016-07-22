context("Sampling data.frames")

test_that("sample_df() works with integers and numbers in (0, 1).", {
  s <- sample_df(data = mtcars, n = 10)
  ss <- sample_df(data = mtcars, n = 0.1)
  expect_equal(nrow(s), 10)
  expect_equal(nrow(ss), 3)
})