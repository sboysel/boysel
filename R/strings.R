#' Remove trailing and leading spaces from a character vector
#'
#' @param x a character vector
#' @return a version of \code{x} without trailing OR leading spaces
#'
#' @examples
#' trim("foo ")
#' trim(" bar ")
#' trim("baz")
#'
#' @seealso \code{\link[stringr]{str_trim}}
#'
#' @export
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

#' Remove trailing newline delimeters from a character vector
#'
#' @param x a character vector
#' @return a version of \code{x} without trailing newline delimiters
#'
#' @examples
#' chomp("foo\n")
#' chomp("bar")
#'
#' @export
chomp <- function(x) gsub("\\n+$", "", x)

#' Wrap a string using newline delimeters
#'
#' @param x a character vector
#' @param n an integer of length 1.  \code{text_wrap()} will silently coerce
#' any numeric value passed to \code{n} to an integer.
#' @return a version of x in which each element substitutes newline delimiters
#' for the immediate next space (e.g. ' ') after every \code{n}th character.
#'
#' @examples
#' s <- "The quick brown fox jumped over the lazy dog."
#' text_wrap(s)
#' text_wrap(s, 10)
#' cat(paste0(text_wrap(s), "\n"))
#'
#' @references I attribute the regular expression that powers \code{text_wrap()} 
#' to StackOverflow user xiechao: 
#' \url{http://stackoverflow.com/a/2352006/3277821}
#' @export
text_wrap <- function(x, n = 20L) {
  stopifnot(is.character(x), is.numeric(n), length(n) == 1)
  if (!is.integer(n)) {
    n <- as.integer(n)
  }
  xx <- gsub(paste0('(.{1,', n,'})(\\s|$)'), '\\1\n', x)
  chomp(xx)
}


