library(boysel)
context("Utilities")

test_that("count function, n()", {
  x <- c(1, 2, 3, NA)
  y <- c(NA)
  expect_equal(n(x), 3)
  expect_equal(n(y), 0)
})

test_that("things get shuffled", {
  x <- factor(letters)
  y <- 1:26
  z <- letters
  dd <- data.frame(x = x, y = y, z = z)
  ll <- list(x = x, y = y, z = z)
  expect_equal(length(x), length(shuffle(x)))
  expect_equal(length(y), length(shuffle(y)))
  expect_equal(length(z), length(shuffle(z)))
  expect_equal(length(dd), length(shuffle(dd)))
  expect_equal(length(ll), length(shuffle(ll)))
  expect_identical(class(x), class(shuffle(x)))
  expect_identical(class(y), class(shuffle(y)))
  expect_identical(class(z), class(shuffle(z)))
  expect_identical(class(dd), class(shuffle(dd)))
  expect_identical(class(ll), class(shuffle(ll)))
  expect_equal(mean(y), mean(shuffle(y)))
  expect_identical(letters, sort(shuffle(letters)))
})

test_that("clearing the workspace functions properly", {
  e <- new.env()
  # assign("x", "foo", envir = e)
  e$x <- "foo"
  expect_true("x" %in% ls(envir = e))
  expect_message(clear(envir = e))
  expect_false("x" %in% ls(envir = e))
})

test_that("catf() spits out files", {
  tmp <- tempfile()
  cat("foo\n", "bar\n", "baz\n", file = tmp)
  expect_output_file(catf(tmp), file = tmp)
  unlink(tmp)
})

test_that("symdiff behaves", {
  x <- c(1, 2, 3, 4)
  y <- c(1, 4, 5)
  expect_equal(symdiff(x, y), c(2, 3, 5))
  x <- letters[1:4]
  y <- letters[2:5]
  expect_equal(symdiff(x, y), c("a", "e"))
})

test_that("temp_dir functions properly", {
  tmp <- temp_dir()
  expect_length(tmp, 1)
  expect_is(tmp, "character")
  expect_true(dir.exists(tmp))
  expect_length(list.files(tmp), 0)
  unlink(tmp, force = TRUE, recursive = TRUE)
  expect_false(dir.exists(tmp))
})