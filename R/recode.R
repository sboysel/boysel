#' Flexible 
#' 
#' Collapse multiple original values of a vector into single new values.  An
#' alternative to nested \code{ifelse} statements, \code{dplyr::recode},
#' \code{plyr::revalue}, etc.
#'
#' @param x a vector
#' @param ... a sequence of arguments where the name is the replacement and the
#' value are the elements to be replaced.  This is opposite the behavior of
#' \code{dplyr::recode} and \code{plyr::revalue}.
#' @param one for \code{binary_recode}, all elements of \code{x} in \code{one}
#' will be assigned 1, anything in \code{.na} will be assigned NA, and anything
#' left over will be assigned 0.
#' @param .default the value is assigned to all elements of \code{x} not 
#' matching any of the replacement values in \code{...} (i.e. a catch-all 
#' replacement value for anything not explicitly mapped).
#' @param .na all values in \code{.na} will be assigned NA.
#' @param factor If TRUE, returned the recoded version of \code{x} as a factor.
#' Defaults to FALSE, in which all non-NA elements are forced to character.
#' @return a vector of the same length as \code{x} in which all substitutions
#' specified in \code{...} (or \code{one} in \code{binary_recode}) are made.
#'
#' @examples
#' s <- c("a", "b", "c", "d", "e", "f", NA)
#' recode(s, `1` = c("a", "b"), .default = 0)
#' recode(s, `1` = c("a", "b"), .na = c(NA, "d", "e"))
#' recode(s, `1` = c("a", "b"))
#' recode(s, `1` = c("a", "b"), factor = TRUE)
#' binary_recode(s, one = c("a", "b"))
#'
#' @name recode

#' @export
#' @rdname recode
recode <- function(x, ..., .default = NULL, .na = NA, factor = FALSE) {
  recodes <- list(...)
  stopifnot(length(.default) == 1 | is.null(.default), is.logical(factor),
            length(base::intersect(unlist(recodes), .na)) == 0)

  xx <- vector(length = length(x))
  
  na_inds <- which(x %in% .na)
  default_inds <- which(!(x %in% unlist(recodes)) & !(x %in% .na))

  xx[na_inds] <- NA

  if (is.null(.default)) {
    message("No default value set")
    xx[default_inds] <- x[default_inds]
  } else {
    xx[default_inds] <- .default
  }

  for (nm in names(recodes)) {
    xx[which(x %in% recodes[[nm]])] <- nm
  }
 
  if (factor) {
    factor(xx)
  } else {
    xx
  }
}

#' @export
#' @rdname recode
binary_recode <- function(x, one, .na = NA) {
  as.integer(recode(x, `1` = one, .default = 0, .na = .na, factor = FALSE))
}


