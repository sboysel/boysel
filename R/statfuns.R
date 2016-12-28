#' Coefficient of Variation
#'
#' Calculate the coefficient of variation (i.e. variation coefficient) for
#' a vector.
#'
#' @param x a numeric vector
#' @param na.rm Passed directly to \code{sd()} and \code{mean()}.
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

#' Statistical Mode
#' 
#' Calculate most frequently occuring value in a vector.
#' 
#' @param x a vector
#' @param na.rm logical If TRUE, all NA values will be dropped.
#' @return a scalar equal to the modal value of \code{x}.  If \code{x} is multimodal, \code{length(x)}
#' will equal the number of unique modes in \code{x}.
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
#' @export
#' @rdname most_freq
most_freq <- function(x, na.rm = TRUE) {
  UseMethod("most_freq")
}

#' @export
#' @rdname most_freq
most_freq.numeric <- function(x, na.rm = TRUE) {
  tab <- table(x)
  m <- which(tab == max(tab, na.rm = TRUE), arr.ind = TRUE)
  as_numeric(names(tab)[m])
}

#' @export
#' @rdname most_freq
most_freq.character <- function(x, na.rm = TRUE) {
  tab <- table(x)
  m <- which(tab == max(tab, na.rm = TRUE), arr.ind = TRUE)
  names(tab)[m]
}

#' @export
#' @rdname most_freq
most_freq.factor <- function(x, na.rm = TRUE) {
  tab <- table(x)
  m <- which(tab == max(tab, na.rm = TRUE), arr.ind = TRUE)
 factor(names(tab)[m])
}
