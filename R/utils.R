#' Count the number of non-NA observations in a vector
#'
#' @param x a vector
#'
#' @return the number of non-NA observations in \code{x} as an integer.
#'
#' @export
n <- function(x) {
  length(x[!is.na(x)])
}

#' Shuffle an object
#'
#' @param x Either a vector, list, matrix, or \code{data.frame}
#'
#' @return an object of the same type as \code{x} with elements ordered in a 
#' random permutation.
#'
#' @examples
#' shuffle(1:5)
#' shuffle(letters[1:5])
#' shuffle(as.list(letters[1:5]))
#' shuffle(diag(5))
#' shuffle(mtcars)
#'
#' @rdname shuffle
#' @export
shuffle <- function(x) UseMethod("shuffle")
#' @export
shuffle.default <- function(x) {
  x[sample(1:length(x))]
}
#' @export
shuffle.list <- function(x) {
  x[sample(1:length(x))]
}
#' @export
shuffle.data.frame <- function(x) {
  x[sample(1:nrow(x)), ]
}
#' @export
shuffle.matrix <- function(x) {
  x[sample(1:nrow(x)), ]
}

#' Remove all objects in an environment
#'
#' An alias for \code{rm(list = ls())} for a specified environment. Use with 
#' care.
#'
#' @param envir an environment from which all objects will be removed.  Default
#' is \code{.GlobalEnv} (i.e. ''the user's workspace'').
#'
#' @examples
#' x <- 1
#' y <- 2
#' x <- 3
#' ls()
#' clear()
#' ls()
#'
#' @export
clear <- function(envir = globalenv()) {
  message(paste("Removing all objects from", environmentName(envir)))
  rm(list = ls(envir = envir), envir = envir)
}

#' Print the contents of a file
#'
#' Wraps \code{\link{cat}} to simple print the contents of a file as output.
#'
#' @param f a filename passed to \code{\link{readLines}}.
#' @return The contents of \code{f} are displayed as output.
#'
#' @examples
#' \dontrun{
#' writeLines(text = c("a", "b", "c"), con = "foo")
#' catf("foo")
#' }
#'
#' @references Attributed to Yihui Xie via StackOverflow:
#' \url{http://stackoverflow.com/a/29264573/3277821} 
#' @export
catf <- function(f) {
  cat(readLines(con = f, warn = FALSE), sep = "\n")
}

