#' Subtract mean values by grouping factor
#' 
#' Subtract mean values by grouping factor. Accepts either two vectors (data and groups) or 
#' a \code{data.frame} and a formula of the form \code{data ~ groups}.
#' 
#' @param x a numeric vector.
#' @param groups a vector of the same length as \code{x} that contains a group indicator 
#' corresponding to each element of \code{x}.
#' @param f a formula.  For use with the \code{data.frame} \code{data}.  Syntax is 
#' \code{x ~ groups} where \code{x} is a numeric column of \code{data} that is grouped
#' by \code{groups}.
#' @param data a \code{data.frame}.
#' @param digits an integer passed directly to \code{\link{round}}.
#' @param named If \code{TRUE}, the names of the ouput vector are set to the original 
#' values of the numeric input data.
#' @param ... arguments passed to specific methods.
#' @return A vector of the same length as the input data (either \code{x} or the input 
#' column from the \code{data.frame} \code{data}) where group means have been subtracted 
#' from each element in the input data.  Specifically, residuals are reported the ordinary
#' least squares regression \code{lm(x ~ factor(groups))}.
#'
#' @examples
#' # using a numeric vector and corresponding grouping vector
#' x <- runif(50)
#' g <- rep(letters[1:5], 10)
#' group_demean(x, g)
#' # using a formula and data.frame
#' group_demean(mpg ~ cyl, mtcars)
#'
#' @name group_demean
#'
#' @export
#' @rdname group_demean
group_demean <- function(...) UseMethod("group_demean")

#' @export
#' @rdname group_demean
group_demean.numeric <- function(x, groups, digits = getOption("digits"), named = FALSE, ...) {
  stopifnot(is.numeric(x), length(x) == length(groups))
  
  data <- data.frame(x = x, g = groups)
  
  f <- stats::as.formula(
    "x ~ 0 + factor(g)",
    env = parent.frame()
  )
  
  r.rounded <- group_demean.formula(f = f, data = data, digits = digits, named = named, ...)
  
  if (named) {
    names(r.rounded) <- x
  }
  
  r.rounded
}

#' @export
#' @rdname group_demean
group_demean.formula <- function(f, data, digits = getOption("digits"), named = FALSE, ...) {
  # f is specified as follows:
  # data ~ groups
  stopifnot(is_formula(f), is.data.frame(data))
  
  terms <- base::all.vars(expr = f)
  
  f <- stats::as.formula(
    paste(terms[1], "~ 0 +", paste0("factor(", terms[2], ")")),
    env = parent.frame()
  )
  
  fit <- stats::lm(f, data = data)
  r <- stats::residuals(fit)
  r.rounded <- round(r, digits = digits)
  
  if (!named) {
    names(r.rounded) <- NULL
  }
  
  r.rounded
}
