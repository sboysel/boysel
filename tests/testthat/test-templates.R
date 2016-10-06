# Source:
# https://github.com/rstudio/rticles/blob/master/tests/testthat/test_formats.R

context("R Markdown Templates")

test_format <- function(name, file_check = TRUE, os_skip = NULL) {
  
  test_that(paste(name, "format"), {
    
    # work in a temp directory
    dir <- tempfile()
    dir.create(dir)
    oldwd <- setwd(dir)
    on.exit(setwd(oldwd), add = TRUE)
    
    # create a draft of the format
    testdoc <- "testdoc.Rmd"
    rmarkdown::draft(testdoc,
                     system.file("rmarkdown", "templates", name,
                                 package = "boysel"),
                     create_dir = FALSE,
                     edit = FALSE)
    
    # render it
    capture.output({
      if (file_check) {
        output_file <- tempfile(fileext = ".pdf")
        rmarkdown::render(testdoc, output_file = output_file)
        expect_true(file.exists(output_file))
      } else {
        rmarkdown::render(testdoc)
      }
    })
  })
}

test_format("article")
