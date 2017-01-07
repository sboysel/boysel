#' Flexible vector normalization
#' 
#' Rescale (normalize) a vector
#' 
#' @param x a numeric vector
#' @param fun either a character vector of length one equal to one of \code{'scale'}, 
#' \code{'unit.range'} (see Details), or \code{''}, or a function of \code{x} defined 
#' such that the parameters in \code{...} are passed through.  For example, 
#' \code{fun = function(x, ...) log(x, ...)}.
#' @param ... additional arguments passed to \code{fun}.
#' @return a numeric vector of length equal to \code{x} where each original element
#' of \code{x} has been transformed according to the specification in \code{fun}.
#' 
#' @details 
#' \describe{
#'   \item{\code{'scale'}}{\code{base::scale(x, center = TRUE, scale = TRUE)}}
#'   \item{\code{'unit.range'}}{\code{function(x, ...) = (x - min(x, ...)) / (max(x, ...) - min(x, ...))}}
#'   \item{\code{'unit.length'}}{\code{function(x) = x / norm(x, type = "2")}}
#' }
#' 
#' @references \url{https://en.wikipedia.org/wiki/Normalization_(statistics)}
#' 
#' @export
normalize <- function(x, fun = c("scale", "unit.length", "unit.range"), ...) {
  stopifnot(is.numeric(x))
  if (is.function(fun)) {
    fun(x, ...)
  } else {
    fun <- match.arg(fun)
    if (fun == "unit.length" & NA %in% x) {
      stop("Cannot rescale x to unit length with NA values")
    }
    switch(fun,
           scale = scale(x, ...),
           unit.length = x / norm(x, type = "2"),
           unit.range = (x - min(x, ...)) / (max(x, ...) - min(x, ...))
    )
  }
}

