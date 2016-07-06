#' Coefficient of Variation
#'
#' Calculate the coefficient of variation (i.e. variation coefficient) for
#' a vector.
#'
#' @param x a numeric vector
#' @param na.rm Passed directly to \code{sd} and \code{mean}.
#' @return a scalar equal to sd(x) / mean(x).
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


#' Simple summary statistics
#'
#' Generate a simple set of summary statistics for the numeric columns of a
#' data.frame over various subsets.  There are MANY other ways to do this but
#' this function is suitable to my needs.
#'
#' @param data a data.frame
#' @param f a formula (See \code{\link[stats]{aggregate}} for details).
#' @param digits an integer to specify the number of decimal places desired for
#' the resulting summary statistics.  Passed directly to \code{round}.
#' @param order if TRUE, the rows of the resulting data.frame will be sorted
#' alphabetically.  If FALSE, the rows will be sorted alphabetically according
#' to the permutations of the grouping factors in by.
#' @return a data.frame
#'
#' @examples
#' sumstats(mtcars)
#' sumstats(iris)
#' sumstats(iris, cbind(Petal.Width, Sepal.Width) ~ NULL)
#' sumstats(iris, . ~ Species)
#' sumstats(mtcars, cbind(mpg, wt) ~ cyl + vs)
#' sumstats(mtcars, . ~ cyl + vs)
#' @export
sumstats <- function(data, f = NULL, digits = 2, order = TRUE) {
  # TODO:
  # - sumstats: add subset parameters                                   [ ]
  if (is.null(f)) {
    data <- data[sapply(data, is.numeric)]
    summ <- sapply(data, function(x) {
      round(sumstats_row(x), digits = digits)
    })
    summ <- data.frame(t(summ))
    summ.names <- row.names(summ)
    summ.grps <- character()
  } else {
    summ <- stats::aggregate(f, data, function(x) {
      round(sumstats_row(x), digits = digits)
    })
    summ.grps <- labels(stats::terms(f))
    summ.names <- base::setdiff(names(lapply(summ, unlist)), summ.grps)
    if (length(summ.grps) > 0) {
      summ.list <- as.list(summ)[summ.names]
      summ <- lapply(summ.list, function(x) {
        data.frame(cbind(summ[summ.grps], x))
      })
    } else {
      summ <- lapply(summ, unlist)
    }
    summ <- data.frame(do.call(rbind, summ))
  }
  # Add a variable name column
  summ$variable <- summ.names
  # Place variable and grouping factors first
  reordered.cols <- c("variable", base::setdiff(names(summ), "variable"))
  summ <- summ[reordered.cols]
  if (order) {
    summ <- summ[do.call("order", as.list(summ[c("variable", summ.grps)])), ]
  }
  row.names(summ) <- NULL
  summ
}

sumstats_row <- function(x) {
  c(n = length(x[!is.na(x)]),
    mean = mean(x, na.rm = TRUE),
    sd = stats::sd(x, na.rm = TRUE),
    # cv = cv(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    p25 = stats::quantile(x, probs = 0.25, na.rm = TRUE, names = FALSE),
    p50 = stats::quantile(x, probs = 0.5, na.rm = TRUE, names = FALSE),
    p75 = stats::quantile(x, probs = 0.75, na.rm = TRUE, names = FALSE),
    max = max(x, na.rm = TRUE))
}
