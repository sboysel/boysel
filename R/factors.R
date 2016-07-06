#' Converting factors to numeric
#'
#' @param x a vector
#'
#' @return a numeric vector of the same length as x
#'
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

