library(boysel)
context("reshape wrappers")

test_that("wrappers work on toy examples", {
  t <- "id,sex,inc80,inc81,inc82
  1,0,5000,5500,6000
  2,1,2000,2200,3300
  3,0,3000,2000,1000"
  df1 <- read.csv(text = t, strip.white = TRUE)
  df2 <- wide_to_long(data = df1, stub = "inc", i = "id", j = "year")
  df3 <- long_to_wide(data = df2, stub = "inc", i = "id", j = "year")
  expect_is(df2, "data.frame")
  expect_is(df3, "data.frame")
  expect_identical(dim(df2), c(9L, 4L))
  expect_identical(dim(df3), dim(df1))
  expect_identical(df1, df3)
  df4 <- long_to_wide(data = df2, stub = "inc", i = "id", j = "year",
                      clean = FALSE)
  expect_false(identical(df1, df4))
})