#' Reshape the Stata way
#' 
#' Wraps the \code{R} function \code{reshape} to mimic the behavior of the 
#' Stata \code{reshape}.  Used to transform data.frames between "wide" and
#' "long" formats.
#'
#' @param data a data.frame
#' @param stub a character string.  For \code{wide_to_long}, this is the
#' leading character "stub" common to all varying columns to be collected into
#' rows.  For \code{long_to_wide}, this is the name of the variable to be
#' spread into columns.
#' @param i a string
#' @param j a string
#' @param clean a boolean value.  If TRUE, the resulting data.frame is tidied
#' automatically: attribute information from \code{reshape} is removed, row
#' names are reset to a numeric sequence, and column names are stripped of
#' dots.  Set to FALSE if you would like the immediate result from
#' \code{reshape}.
#' @return a data.frame.
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
