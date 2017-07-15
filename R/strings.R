#' String / text processing functions
#'
#' @param x a character vector
#' @param n an integer of length 1.  \code{text_wrap()} will silently coerce
#' any numeric value passed to \code{n} to an integer.
#' @param ... addtional parameters used by specific functions.  See Details.
#' 
#' @details \itemize{
#'   \item{\code{trim}}{returns a version of \code{x} without trailing OR leading spaces.}
#'   \item{\code{chomp}}{returns a version of \code{x} without trailing newline delimiters.}
#'   \item{\code{text_wrap}}{returns a version of x in which each element substitutes newline 
#'   delimiters for the immediate next space (e.g. ' ') after every \code{n}th character.}
#'   \item{\code{translit}}{returns a version of \code{x} recoded to ASCII, using \code{iconv} and 
#'   \code{//TRANSLIT}.  Parameters in \code{...} are passed directly to \code{iconv}.}
#'   \item{\code{rand_string}}{returns a randomly generated alphanumeric string of length \code{n}.}
#' }
#' 
#' @references I attribute the regular expression that powers \code{text_wrap()} 
#' to StackOverflow user xiechao: 
#' \url{http://stackoverflow.com/a/2352006/3277821}
#'
#' @examples
#' trim("foo ")
#' trim(" bar ")
#' trim("baz")
#' chomp("foo\n")
#' chomp("bar")
#' s <- "The quick brown fox jumped over the lazy dog."
#' text_wrap(s)
#' text_wrap(s, 10)
#' cat(paste0(text_wrap(s), "\n"))
#' rand_string()
#' rand_string(5)
#'
#' @name strings

#' @rdname strings
#' @export
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

#' @rdname strings
#' @export
chomp <- function(x) gsub("\\n+$", "", x)

#' @rdname strings
#' @export
text_wrap <- function(x, n = 20L) {
  stopifnot(is.character(x), is.numeric(n), length(n) == 1, n >= 1)
  if (!is.integer(n)) {
    n <- as.integer(n)
  }
  xx <- gsub(paste0('(.{1,', n,'})(\\s|$)'), '\\1\n', x)
  chomp(xx)
}

#' @rdname strings
#' @export
translit <- function(x, ...) {
  iconv(x = x, to = "ASCII//TRANSLIT", ...)
}

#' @rdname strings
#' @export
rand_string <- function(n) {
  paste0(sample(c(letters, LETTERS, 0:9),
                size = n, replace = TRUE), collapse = "")
}
