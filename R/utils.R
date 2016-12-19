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
  stopifnot(is.environment(envir))
  if (identical(environmentName(envir), "")) {
    n <- paste(deparse(substitute(e)), "(not named)")
  } else {
    n <- environmentName(envir)
  }
  message(paste("Removing all objects from", n))
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

#' Return matching elements in a vector
#' 
#' @param x a character vector with elements to be matched
#' @param pattern a regular expression.  If \code{length(pattern) > 1}, each element 
#' is concatenated and a search using \href{https://en.wikipedia.org/wiki/Regular_expression#Basic_concepts}{alternation}.
#' @return a character vector with the elements of \code{x} that match \code{pattern}
#' 
#' @examples 
#' xx <- c("foo", "bar", "baz")
#' matches(x = xx)
#' matches(x = xx, pattern = "foo")
#' matches(x = xx, pattern = "(foo|bar)")
#' # should be equivalent to
#' matches(x = xx, pattern = c("foo", "bar"))
#' matches(x = xx, pattern = "^b.*")
#' 
#' @export
matches <- function(x, pattern = ".", ...) {
  stopifnot(is.character(x), is.character(pattern))
  if (length(pattern) > 1) {
    pattern <- paste0("(", paste0(pattern, collapse = "|"), ")")
  }
  x[grepl(pattern = pattern, x = x, ...)]
}

