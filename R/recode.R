#' Flexible vector recoding 
#' 
#' Collapse multiple original values of a vector into single new values.  An
#' alternative to nested \code{ifelse()} statements, \code{dplyr::recode()},
#' \code{plyr::revalue()}, \code{car::recode()}, etc.
#'
#' @param x a vector
#' @param ... a sequence of arguments where the name is the replacement and the
#' value are the elements to be replaced.  This is opposite the behavior of
#' \code{dplyr::recode()} and \code{plyr::revalue()}.
#' @param one for \code{binary_recode()}, all elements of \code{x} in \code{one}
#' will be assigned 1, anything in \code{.na} will be assigned NA, and anything
#' left over will be assigned 0.
#' @param .default a vector of length 1.  The value is assigned to all elements 
#' of \code{x} not matching any of the replacement values in \code{...} or
#' \code{.na} (i.e. a catch-all replacement value for anything not explicitly 
#' mapped).  If NULL (default), elements of \code{x} without a mapping in 
#' \code{...} or \code{.na} remain the same (i.e. no recoding).
#' @param .na a vector.  All values in \code{.na} will be assigned \code{NA}.  
#' Default is that any \code{NA} values in \code{x} remain \code{NA}.
#' @param factor If TRUE, returned the recoded version of \code{x} as a factor.
#' Defaults to FALSE, in which all non-NA elements are forced to character.
#' @return a vector of the same length as \code{x} in which all substitutions
#' specified in \code{...} (or \code{one} in \code{binary_recode()}) are made.
#' \code{binary_recode()} always returns a numeric vector while \code{recode()}
#' returns a vector coerced according to the various recoding arguments provided.
#'
#' @details
#' Further information on \code{recode()}'s primary value mapping arguments:
#' \describe{
#'   \item{\code{...}}{Any number of named arguments in which for each
#'   argument, any values occurring in \code{x} will be recoded to the name of
#'   the argument.  There are no defaults in \code{recode()}, so \code{recode(x)}
#'   with no additional arguments should be equivalent to \code{identity(x)}}
#'   \item{\code{.na}}{Any values of \code{x} matching values in the vector
#'   provided to \code{.na} will be recoded to \code{NA}.}
#'   \item{\code{.default}}{Any values of \code{x} not matched by either
#'   \code{...} (or \code{one}) or \code{.na} will be recoded to the value
#'   passed to \code{.default}.}
#' }
#' \code{binary_recode()} wraps \code{recode()} but accepts only a single
#' recoding argument (\code{one}) instead of \code{...}), sets
#' \code{.default} to \code{0}, and always returns an integer vector.
#'
#' @examples
#' s <- c("a", "b", "c", "d", "e", "f", NA)
#' recode(s, `1` = c("a", "b"), .default = 0)
#' recode(s, `1` = c("a", "b"), .na = c(NA, "d", "e"))
#' recode(s, `1` = c("a", "b"))
#' recode(s, `1` = c("a", "b"), factor = TRUE)
#' binary_recode(s, one = c("a", "b"))
#'
#' @seealso \code{\link[dplyr]{recode}}, \code{\link[plyr]{revalue}}, 
#' \code{\link[car]{recode}}. 
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


