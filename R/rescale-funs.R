#' Numeric vector rescaling functions
#' 
#' Rescale a numeric vector
#' 
#' @param x a numeric vector
#' @param na.rm if \code{TRUE}, \code{unit_length} calculates the L2 norm based on the non-missing
#' elements of \code{x}.
#' @param ... additional arguments passed to \code{\link{scale}} for \code{center}
#' or \code{\link{max}} and \code{\link{min}} for \code{unit_range}.
#' @return a rescaled version of \code{x}.
#' 
#' @details 
#' \describe{
#'   \item{\code{center}}{\eqn{\frac{x - mean(x)}{sd(x)}}}
#'   \item{\code{unit_range}}{\eqn{\frac{x - min(x)}{max(x) - min(x)}}}
#'   \item{\code{unit_length}}{\eqn{\frac{x}{|| x ||}}}
#' }
#' 
#' @references \url{https://en.wikipedia.org/wiki/Normalization_(statistics)}
#' 
#' @examples 
#' center(1:10)
#' unit_range(1:10)
#' unit_length(1:10)
#' 
#' @name scaling-functions

#' @rdname scaling-functions
#' @export
center <- function(x, ...) {
  as.numeric(scale(x, ...))
}

#' @rdname scaling-functions
#' @export
unit_range <- function(x, ...) {
  (x - min(x, ...)) / (max(x, ...) - min(x, ...))
}

#' @rdname scaling-functions
#' @export
unit_length <- function(x, na.rm = TRUE) {
  
  if (!na.rm & any(is.na(x))) {
    stop("x contains NA values and na.rm = FALSE")
  } else {
    x / norm(x[!is.na(x)], type = "2")
  }
  
}

