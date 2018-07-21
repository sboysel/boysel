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
#' 
#' is_char_numeric(c(1, 3, "3", NA))
#' is_char_numeric(c(1, 3, "3", NA, "A"))
#'  
#' is_formula("y ~ x")
#' is_formula(as.formula("y ~ x"))
#' 
#' @name logical
#' 
#' @export
#' @rdname logical
is_char_numeric <- function(x) {
  sapply(
    X = x,
    FUN = function(z) {
      xx <- utils::type.convert(x = z)
      if (is.na(xx)) {
        NA
      } else {
        is.numeric(xx)
      }
    },
    USE.NAMES = FALSE,
    simplify = TRUE
  )
}

#' @export
#' @rdname logical
is_formula <- function(x) {
  if (is.list(x)) {
    sapply(
      x,
      function(z) as.logical(inherits(z, what = "formula", which = TRUE)),
      USE.NAMES = FALSE,
      simplify = TRUE
    )
  } else {
    as.logical(inherits(x, what = "formula", which = TRUE))
  }
}
