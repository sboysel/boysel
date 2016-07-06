context("String utilities")

test_that("chomp and trim function properly", {
  s <- c(" a", "b ", " c ", "      d", "  e   ")
  expect_equal(5, sum(nchar(trim(s))))
  expect_identical(letters[1:5], trim(s))
  s <- c("\n", "a\n", "b\n", "c")
  expect_identical(c("", "a", "b", "c"), chomp(s))
})
