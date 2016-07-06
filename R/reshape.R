#' Reshape the Stata way
#' 
#' Wraps the function \code{base::reshape()} to mimic the behavior of the 
#' Stata \code{reshape}.  Used to transform data.frames between "wide" and
#' "long" formats.
#'
#' @param data a \code{data.frame}
#' @param stub a character string.  For \code{wide_to_long()}, this is the
#' leading character "stub" common to all varying columns to be collected into
#' rows.  For \code{long_to_wide()}, this is the name of the variable to be
#' prefixed to the values of \code{j} and spread into columns.
#' @param i a string.  The name of the column in \code{data} that uniquely
#' identifies each observation.
#' @param j a string.  For \code{wide_to_long()}, this will be the name for the
#' new column that captures the values of the varying colums that follow the
#' value passed to \code{stub}.  For \code{long_to_wide()}, this is the name of
#' an existing column, the values of which we be suffixed to \code{stub} as the
#' observations are spread into 'wide' format.
#' @param clean a logical value.  If TRUE, the resulting \code{data.frame} is 
#' tidied automatically: attribute information from \code{reshape()} is 
#' removed, row names are reset to a numeric sequence, and column names are 
#' stripped of dots.  Set to FALSE if you would like the immediate result from
#' \code{reshape()}.
#' @return a \code{data.frame}.
#'
#' @details \code{wide_to_long()} and \code{long_to_wide()} are designed to
#' closely mimic basic functionality of Stata's \code{reshape} command.  See
#' the examples below for intended usage.
#'
#' @examples
#' t <- "id,sex,inc80,inc81,inc82
#' 1,0,5000,5500,6000
#' 2,1,2000,2200,3300
#' 3,0,3000,2000,1000"
#' df1 <- read.csv(text = t)
#' df2 <- wide_to_long(data = df1, stub = "inc", i = "id", j = "year")
#' df3 <- long_to_wide(data = df2, stub = "inc", i = "id", j = "year")
#' identical(df1, df3)
#' df4 <- long_to_wide(data = df2, stub = "inc", i = "id", j = "year",
#'                     clean = FALSE)
#' identical(df1, df4)
#'
#' @references \url{http://www.stata.com/manuals13/dreshape.pdf}
#' @name reshape_stata

#' @export
#' @rdname reshape_stata
wide_to_long <- function(data, stub, i, j, clean = TRUE) {
  stopifnot(i %in% names(data))
  varying <- grep(stub, names(data))
  names(data)[varying] <- dot_split(names(data)[varying])
  r <- stats::reshape(data,
                      varying = varying,
                      timevar = j,
                      idvar = i,
                      direction = "long")
  if (clean) {
   row.names(r) <- NULL
   attr(r, "reshapeWide") <- NULL 
  }
  r
}

#' @export
#' @rdname reshape_stata
long_to_wide <- function(data, stub, i, j, clean = TRUE) {
  stopifnot(all(c(stub, i, j) %in% names(data)))
  idvars <- base::setdiff(names(data), c(stub, j))
  r <- stats::reshape(data,
                      timevar = j,
                      idvar = idvars,
                      direction = "wide")
  if (clean) {
   names(r) <- gsub("\\.", "", names(r))
   attr(r, "reshapeWide") <- NULL 
  }
  r
}

# Source:
# https://trinkerrstuff.wordpress.com/2012/05/03/reshape-from-base-explained-part-i/
dot_split <- function(x) {
  gsub("([a-z])([0-9])", "\\1\\.\\2", x)
}
