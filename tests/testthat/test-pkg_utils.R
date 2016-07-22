context("Package Utilities")

options(repos = c(CRAN = "https://cran.rstudio.com/"))

test_that("loaded_pkgs()", {
  library(stats)
  expect_true("stats" %in% loaded_pkgs())
  expect_is(loaded_pkgs(), "character")
})

test_that("is_installed()", {
  library(datasets)
  expect_true(is_installed(datasets))
  expect_is(is_installed(datasets), "logical")
  expect_identical(is_installed(datasets), is_installed("datasets"))
})

test_that("lib()", {
  b <- is_installed(magrittr)
  lib(magrittr)
  expect_true(lib(magrittr))
  expect_true(is_installed(magrittr))
  if (!b) remove.packages("magrittr")
})

test_that("in_cran()", {
  s <- in_cran(ggplot2)
  expect_true(s)
  expect_is(s, "logical")
})

test_that("name_to_char()", {
  expect_is(name_to_char(substitute(cat)), "character")
  expect_identical(name_to_char(substitute(cat)), "cat")
  expect_identical(name_to_char(substitute("cat")), "cat")
  expect_identical(name_to_char("cat"), "cat")
  expect_error(name_to_char(1))
})