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

#' Symmetric set difference
#' 
#' @param x a vector
#' @param y a vector of the same type as \code{x}.
#' @return the symmetric difference of set \code{x} and \code{y}.
#' 
#' @references Weisstein, Eric W. "Symmetric Difference." 
#' From MathWorld--A Wolfram Web Resource. 
#' \url{http://mathworld.wolfram.com/SymmetricDifference.html}
#' 
#' @examples
#' x <- c(1, 2, 3, 4)
#' y <- c(1, 4, 5)
#' symdiff(x, y)
#' x <- letters[1:4]
#' y <- letters[2:6]
#' symdiff(x, y)
#' 
#' @export
symdiff <- function(x, y) {
  base::union(
    base::setdiff(x, y),
    base::setdiff(y, x)
  )
}

#' Create a temporary directory
#' 
#' Creates a temporary subdirectory of \code{\link{tempdir}}.
#' 
#' @param path base directory in which temporary directory will be created.  Defaults
#' to \code{\link{tempdir}}.
#' @param nchar integer length of random string used as directory name.  Default is
#' \code{10}.
#' @return the full directory path of the temporary directory as a string.
#' 
#' @examples
#' tmp <- temp_dir()
#' tmp
#' dir.exists(tmp)
#' unlink(tmp, force = TRUE)
#'
#' @export
temp_dir <- function(path = tempdir(), nchar = 10L) {
  tmp <- file.path(path, rand_string(n = nchar))
  dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
  tmp
}