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

#' Count the number of dimensions in an object
#'
#' @param x a object
#'
#' @return the number dimensions in \code{x}
#'
#' @export
ndim <- function(x) {
  length(dim(x))
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

#' Remove all objects in the current environment
#'
#' An alias for \code{rm(list = ls())}.  Use with care.
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
clear <- function() {
  rm(list = ls())
}
