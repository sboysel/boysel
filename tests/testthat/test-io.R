context("Data I/O")

test_that("Reading csv from a text string works", {
  s1 <- "x,y\n1,a\n2,b"
  df1 <- read_text(s1)
  s2 <- "x,y
         1,a
         2,b"
  df2 <- read_text(s2, strip.white = TRUE)
  df3 <- data.frame(x = 1:2,
                    y = letters[1:2])
  expect_identical(df, df)
  expect_identical(df, df)
})
