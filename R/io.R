#' Data I/O convenience functions
#'
#' @param text a character vector of length 1.  Should resemble the contents of
#' a CSV file.
#' @param ... additional arguments passed directly to \code{read.csv}
#' @return a data.frame
#'
#' @examples
#' t <- "id,x,y
#'       a,1,2
#'       b,3,4"
#' read_text(t, strip.white = TRUE)
#' t <- "id,x,y\na,1,2\nb,3,4"
#' read_text(t)
#'
#' @export
read_text <- function(text, ...) {
  utils::read.csv(text = text, ...)
}
