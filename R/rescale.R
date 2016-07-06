#' Rescale a numeric vector
#'
#' @param x a numeric vector
#' @param method character string specifying the type of scaling.  Currently 
#' supported options include 'standardize', 'unit.range', and 'unit.length'.
#' @param ... additional parameters passed to specific rescaling functions 
#' (e.g. \code{na.rm}).
#'
#' @return a numeric vector of equal length as x, rescaled according to the 
#' procedure chosen in 'type'.
#'
#' @details
#' Supported rescaling methods:
#' \itemize{
#'   \item 'standardize': scale(x, ...)
#'   \item 'unit.range': (x - min(x, ...)) / (max(x, ...) - min(x, ...))
#'   \item 'unit.length' x / sqrt(sum(x^2, ...))
#' }
#'
#' @examples
#' test <- c(NA, 1, 2, 3)
#' \dontrun{ 
#' # Will fail for undefined rescale method
#' rescale(test, "foo")
#' # Will fail on non-numeric vectors
#' rescale(letters)
#' }
#' rescale(test)
#' rescale(test, "unit.range", na.rm = TRUE)
#' rescale(test, "unit.length", na.rm = TRUE)
#'
#' @export
rescale <- function(x, method = "standardize", ...) {
  stopifnot(is.numeric(x))
  if (! method %in% c("standardize", "unit.range", "unit.length")) {
    stop("method must be one of 'standardize', 'unit.range', and 'unit.length'.")
  }
  switch(method,
         standardize = scale(x, ...),
         unit.range = (x - min(x, ...)) / (max(x, ...) - min(x, ...)),
         unit.length = x / sqrt(sum(x^2, ...))
  )
}

