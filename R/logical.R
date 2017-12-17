#' Logical operators
#'
#' Test an object \code{x}.  See Details for an explanation of how each function operates.
#'
#' @param x an object to check.
#' @return logical
#' @details  \describe{
#'     \item{\code{is_char_numeric}}{TRUE if \code{x} can be converted into a numeric vector by \code{as.numeric} without coercing character elements without a commonly-understood numeric representation.}
#'     \item{\code{is_formula}}{TRUE if \code{x} is a \code{formula} object.}
#' }
#' 
#' @examples
#' x <- c(1, 3, "3", NA)
#' xx <- c(x, "A")
#' is_char_numeric(x)
#' f <- "y ~ x"
#' is_formula(f)
#' ff <- as.formula(f)
#' is_formula(ff)
#' \dontrun{
#' is_char_numeric(xx) # FALSE
#' }
#' 
#' @name logical
#' 
#' @export
#' @rdname logical
is_char_numeric <- function(x) UseMethod("is_char_numeric")

#' @export
#' @rdname logical
is_char_numeric.default <- function(x) {
  sapply(x, function(z) {
    xx <- type.convert(x = z)
    is.numeric(xx) || is.na(xx)
  }, USE.NAMES = FALSE, simplify = TRUE)
}

#' @export
#' @rdname logical
is_formula <- function(x) UseMethod("is_formula")

#' @export
#' @rdname logical
is_formula.formula <- function(x) {
  as.logical(inherits(x, what = "formula", which = TRUE))
}

#' @export
#' @rdname logical
is_formula.vector <- function(x) {
  sapply(x, function(z) as.logical(inherits(z, what = "formula", which = TRUE)))
}
