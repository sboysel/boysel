context("Rescaling vectors")

w <- 1:5
x <- c(w, NA)

test_that("rescale using standardize (base::scale) behaves", {
  y <- rescale(w, method = "standardize")
  z <- rescale(x, method = "standardize")
  expect_equal(w[which(w == mean(w))], attr(y, "scaled:center"))
  expect_equal(sd(y), 1)
  expect_equal(length(x[is.na(x)]), length(z[is.na(z)]))
})

test_that("rescale using 'unit.range' behaves", {
  y <- rescale(w, method = "unit.range")
  z <- rescale(x, method = "unit.range", na.rm = TRUE)
  expect_true(all(max(y) == 1, min(y) == 0,
                  max(z, na.rm = TRUE) == 1, min(z, na.rm = TRUE) == 0))
  expect_equal(length(x[is.na(x)]), length(z[is.na(z)]))
})

test_that("rescale using 'unit.length' behaves", {
  y <- rescale(w, method = "unit.length")
  expect_error(rescale(x, method = "unit.length"))
})


test_that("rescale throws errors for bad arguments", {
  expect_error(rescale(letters))
  expect_error(rescale(1:5, "foo"))
})
