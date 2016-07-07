#' Rescale a numeric vector
#'
#' @param x a numeric vector
#' @param method character string specifying the type of scaling.  Currently 
#' supported options include 'standardize', 'unit.range', and 'unit.length'.
#' @param ... additional parameters passed to specific rescaling functions 
#' (e.g. \code{na.rm}).  Note that if \code{x} contains \code{NA}s, 
#' \code{rescale(x, method = "standardize", na.rm = TRUE)} and \code{rescale(x, 
#' method = "unit.range")} will not drop \code{NA}s in the resulting scaled 
#' vector while \code{rescale(x, method = "standardize")} will stop with an
#' error.
#'
#' @return a numeric vector of equal length as x, rescaled according to the 
#' procedure chosen in 'type'.
#'
#' @details
#' Supported rescaling methods:
#' \describe{
#'   \item{standardize}{scale(x, ...)}
#'   \item{unit.range}{(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
#'   \item{unit.length}{x / norm(x, type = "2")}
#' }
#'
#' @examples
#' test <- c(NA, 1, 2, 3)
#' rescale(test)
#' rescale(test, "unit.range", na.rm = TRUE)
#' test <- test[!is.na(test)]
#' rescale(test, "unit.length")
#' \dontrun{ 
#' # Will fail for undefined rescale method
#' rescale(test, "foo")
#' # Will fail on non-numeric vectors
#' rescale(letters)
#' # 'unit.length' will fail with NA
#' rescale(test, "unit.length", na.rm = TRUE)
#' }
#'  
#'
#' @export
rescale <- function(x, method = "standardize", ...) {
  stopifnot(is.numeric(x))
  if (! method %in% c("standardize", "unit.range", "unit.length")) {
    stop("method must be one of 'standardize', 'unit.range', and 'unit.length'.")
  }
  if (method == "unit.length" & NA %in% x) {
    stop("Cannot rescale x to unit length with NA values")
  }
  switch(method,
         standardize = scale(x, ...),
         unit.range = (x - min(x, ...)) / (max(x, ...) - min(x, ...)),
         unit.length = x / norm(x, type = "2")
  )
}

