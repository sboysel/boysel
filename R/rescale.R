#' Rescale a numeric vector
#'
#' @param x a numeric vector
#' @param type character string specifying the type of scaling.  Currently supported
#' options include 'standardize', 'unit.range', and 'unit.length'.
#' @param ... additional parameters passed to specific rescaling functions (e.g. na.rm)
#'
#' @return a numeric vector of equal length as x, rescaled according to the procedure chosen
#' in 'type'.
#'
#' \itemize{
#'   \item 'standardize': scale(x, ...)
#'   \item 'unit.range': (x - min(x, ...)) / (max(x, ...) - min(x, ...))
#'   \item 'unit.length' x / sqrt(sum(x^2, ...))
#' }
#'
#' @examples
#' test <- c(NA, 1, 2, 3)
#' \dontrun{ 
#' # Undefined rescaling type chosen
#' rescale(test, "foo")
#' # Will fail on non-numeric vectors
#' rescale(letters)
#' }
#' rescale(test)
#' rescale(test, "unit.range", na.rm = TRUE)
#' rescale(test, "unit.length", na.rm = TRUE)
#'
#' @export
rescale <- function(x, type = "standardize", ...) {
  stopifnot(is.numeric(x))
  if (! type %in% c("standardize", "unit.range", "unit.length")) {
    stop("type must be one of 'standardize', 'unit.range', and 'unit.length'.")
  }
  switch(type,
         standardize = scale(x, ...),
         unit.range = (x - min(x, ...)) / (max(x, ...) - min(x, ...)),
         unit.length = x / sqrt(sum(x^2, ...))
  )
}

