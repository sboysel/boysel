context("Group de-mean")

test_that("Group de-mean via linear model is identical to manual method", {
  x <- 1:10
  g <- rep(c("a", "b"), 5)
  gm <- rep(c(5, 6), 5)
  demeaned.manual <- x - gm
  expect_equal(demeaned.manual, group_demean(x = x, groups = g))
  d <- data.frame(x = x, g = g)
  expect_equal(demeaned.manual, group_demean(f = x ~ g, data = d))
})