#' Logical operators
#'
#' Test if character vector can be converted to numeric without loss of 
#' information.
#'
#' @param x a vector
#' @return TRUE if \code{x} can be converted into a numeric vector by 
#' \code{as.numeric} without coercing character elements without a 
#' commonly-understood numeric representation.
#' 
#' @examples
#' x <- c(1, 3, "3", NA)
#' xx <- c(x, "A")
#' is_char_numeric(x)
#' \dontrun{
#' is_char_numeric(xx) # FALSE
#' }
#' @export
is_char_numeric <- function(x) {
  xx <- suppressWarnings(boysel::as_numeric(x))
  length(xx[is.na(xx)]) == length(x[is.na(x)])
}
