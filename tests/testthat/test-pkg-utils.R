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

test_that("lib functions properly", {
  tmp <- temp_dir()
  old_paths <- .libPaths()
  .libPaths(c(tmp, old_paths))
  expect_true(lib(magrittr, lib = tmp, lib.loc = tmp))
  expect_true(is_installed(magrittr, lib.loc = tmp))
  expect_true(lib(magrittr, lib = tmp, lib.loc = tmp))
  .libPaths(old_paths)
  unlink(tmp)
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