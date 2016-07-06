#' Converting factors to numeric
#'
#' @param x a vector
#' @return a numeric vector of the same length as \code{x} in which each value
#' of \code{x} is converted to its actual numeric value
#'
#' @details
#' \code{as_numeric()} is motivated by the need to convert factors to their
#' numerical representation, rather than their numerically encoded levels.  See
#' the examples for differences between \code{base::as.numeric()} and
#' \code{as_numeric}.
#'
#' @examples
#' x <- factor(c(-1.9, 0.2, 3.5))
#' as.numeric(x)
#' as_numeric(x)
#'
#' @seealso \code{as_numeric.factor} directly follows the suggestion from the 
#' \code{Warning} section of \code{\link{factor}}.
#' @name as_numeric

#' @export
#' @rdname as_numeric
as_numeric <- function(x) UseMethod("as_numeric")

#' @export
#' @rdname as_numeric
as_numeric.default <- function(x) as.numeric(x)

#' @export
#' @rdname as_numeric
as_numeric.factor <- function(x) as.numeric(levels(x))[x]

