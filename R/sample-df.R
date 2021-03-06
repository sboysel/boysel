#' Randomly sample a data.frame
#'
#' Sample rows by either specifying the number of desired rows as an integer
#' (e.g. n = 10) or as a fraction (e.g. n = 0.1) of the total number of rows.
#'
#' @param data a \code{data.frame}
#' @param n Either a positive integer or a fraction between 0 and 1
#' @return a subset of randomly selected rows from \code{data}
#'
#' @examples
#' sample_df(mtcars, n = 10)
#' sample_df(mtcars, n = 0.1)
#'
#' @seealso \code{\link[dplyr]{sample_n}}, \code{\link[dplyr]{sample_frac}}.
#' @export
sample_df <- function(data, n) {
  stopifnot(is.data.frame(data), n > 0, n <= nrow(data))
  if (n > 0 & n <= 1) {
    n <- round(nrow(data) * n, 0)
  } 
  data[sample(seq_len(n), n), ]
}
