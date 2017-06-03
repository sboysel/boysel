#' Coefficient of Variation
#'
#' Calculate the coefficient of variation (i.e. variation coefficient) for
#' a vector.
#'
#' @param x a numeric vector
#' @param na.rm Passed directly to \code{sd} and \code{mean}.
#' @return a scalar equal to \eqn{\frac{s_{x}}{\overline{x}}}.
#'
#' @examples
#' x <- 1:5
#' y <- c(x^2, NA)
#' cv(x) > cv(y, na.rm = TRUE)
#'
#' @references  Weisstein, Eric W. "Variation Coefficient." From MathWorld--A
#' Wolfram Web Resource.
#' \url{http://mathworld.wolfram.com/VariationCoefficient.html}
#' @export
cv <- function(x, na.rm = FALSE) {
  stats::sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}

#' Calculate most frequently occuring value in a vector
#' 
#' Calculate most frequently occuring value in a vector (statistical mode).
#' 
#' @param x a vector.
#' @param ... additional parameters passed on to \code{table}.
#' @return a scalar equal to the modal value of \code{x}.  If 
#' \code{x} is multimodal, \code{length(x)} will equal the number 
#' of unique modes in \code{x}.
#' @references \url{http://mathworld.wolfram.com/Mode.html}
#' 
#' @examples
#' x <- c(0, 1, 2, 3, 4, 5, 0)
#' y <- c("foo", "bar", "baz", "foo")
#' z <- factor(y)
#' most_freq(x)
#' most_freq(y)
#' most_freq(z)
#' xx <- c(1, x)
#' most_freq(xx)
#' 
#' @name most_freq
#' @export
most_freq <- function(x, ...) {
  UseMethod("most_freq")
}

#' @rdname most_freq
#' @export
most_freq.numeric <- function(x, ...) {
  tab <- table(x, ...)
  m <- which(tab == max(tab, na.rm = TRUE), arr.ind = TRUE)
  as_numeric(names(tab)[m])
}

#' @rdname most_freq
#' @export
most_freq.character <- function(x, ...) {
  tab <- table(x, ...)
  m <- which(tab == max(tab, na.rm = TRUE), arr.ind = TRUE)
  names(tab)[m]
}

#' @rdname most_freq
#' @export
most_freq.factor <- function(x, ...) {
  tab <- table(x, ...)
  m <- which(tab == max(tab, na.rm = TRUE), arr.ind = TRUE)
  factor(names(tab)[m])
}
