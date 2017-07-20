context("Rescaling functions")

w <- 1:5
x <- c(w, NA)

test_that("center functions properly", {
  y <- center(w)
  z <- center(x)
  expect_length(y, length(w))
  expect_length(z, length(x))
  expect_identical(y, z[!is.na(z)])
  expect_equal(sd(y), 1)
  expect_equal(sd(z, na.rm = TRUE), 1)
  expect_equal(mean(y), 0)
  expect_equal(mean(z, na.rm = TRUE), 0)
  expect_equal(length(x[is.na(x)]), length(z[is.na(z)]))
})

test_that("unit_range functions properly", {
  y <- unit_range(w)
  z <- unit_range(x, na.rm = TRUE)
  zz <- unit_range(x)
  expect_true(all(y >= 0 & y <= 1))
  expect_true(all(z >= 0 & z <= 1, na.rm = TRUE))
  expect_true(all(is.na(zz)))
  expect_length(y, length(w))
  expect_length(z, length(x))
  expect_length(zz, length(zz))
  expect_identical(y, z[!is.na(z)])
  expect_equal(length(x[is.na(x)]), length(z[is.na(z)]))
})

test_that("unit_length functions properly", {
  y <- unit_length(w)
  z <- unit_length(x, na.rm = TRUE)
  expect_true(norm(y, type = "2") == 1)
  expect_true(norm(z[!is.na(z)], type = "2") == 1)
  expect_length(y, length(w))
  expect_length(z, length(x))
  expect_identical(y, z[!is.na(z)])
  expect_equal(length(x[is.na(x)]), length(z[is.na(z)]))
  expect_error(unit_length(x, na.rm = FALSE))
})
