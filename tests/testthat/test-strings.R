library(boysel)
context("String utilities")

test_that("chomp and trim function properly", {
  s <- c(" a", "b ", " c ", "      d", "  e   ")
  expect_equal(5, sum(nchar(trim(s))))
  expect_identical(letters[1:5], trim(s))
  s <- c("\n\n", "a\n", "b\n\n", "\nc", "d")
  expect_identical(c("", "a", "b", "\nc", "d"), chomp(s))
})

test_that("text_wrap functions properly", {
  s <- "hello world"
  expect_identical(s, text_wrap(s))
  expect_identical("hello\nworld", text_wrap(x = s, n = 5L))
  expect_error(text_wrap(1:5)) 
  expect_error(text_wrap(x = "hello", n = "world"))
  expect_error(text_wrap(x = "hello", n = 1:2))
  expect_error(text_wrap(x = "hello", n = -1))
  expect_error(text_wrap(x = "hello", n = 0.5))
  expect_silent(text_wrap(x = "hello", n = 10))
  expect_silent(text_wrap(x = "hello", n = 10L))
})

test_that("translit functions properly (personal macOS machine)", {
  skip_on_travis()
  s <- c("dó", "dà", "trí", "trì") 
  # based on expected results for macOS with libiconv
  expect_equal(translit(s), c("d'o", "d`a", "tr'i","tr`i"))
})

test_that("translit functions properly (Travis CI)", {
  skip_on_os(os = "mac")
  s <- c("dó", "dà", "trí", "trì") 
  # based on expected results for Travis build image with glibc
  expect_equal(translit(s), c("do", "da", "tri","tri"))
})

test_that("rand_string functions properly", {
  expect_is(rand_string(sample(1:10, 1)), "character")
  expect_length(rand_string(10), 1)
  expect_equal(nchar(rand_string(10)), 10)
})
