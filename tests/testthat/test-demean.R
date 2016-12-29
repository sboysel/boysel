context("Group de-mean")

x <- 1:10
g <- rep(c("a", "b"), 5)
d <- data.frame(x = x, g = g)

gm <- rep(c(5, 6), 5)
demeaned.manual <- x - gm

test_that("Group de-mean via linear model is identical to manual method", {
  expect_equal(demeaned.manual, group_demean(x = x, groups = g), tolerance = .Machine$double.eps ^ 0.5)
  expect_equal(demeaned.manual, group_demean(f = x ~ g, data = d), tolerance = .Machine$double.eps ^ 0.5)
})

test_that("Vectors are returned", {
  expect_is(group_demean(x = x, groups = g), "numeric")
  expect_is(group_demean(f = x ~ g, data = d), "numeric")
})

test_that("Retaining names works", {
  xx <- group_demean(x = x, groups = g, named = TRUE)
  dd <- group_demean(f = x ~ g, data = d, named = TRUE)
  expect_equal(names(xx), as.character(x))
  expect_equal(names(dd), as.character(d$x))
})