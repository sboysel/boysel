#' Package utility functions
#' 
#' Several convenience functions when working with loading, installing, or
#' querying package database.
#'
#' @param pkg a package name either as a string or as a bare (i.e. unquoted) 
#' name.
#'
#' @details \code{loaded_pkgs()} returns a vector of attached packages.
#' \code{is_installed(pkg)} returns a logical value indicating if \code{pkg} is
#' installed.  \code{lib(pkg)} checks to see if \code{pkg} is installed,
#' installs \code{pkg} if it is not, then loads \code{pkg}.  \code{in_cran(pkg)}
#' returns \code{TRUE} if \code{pkg} is listed on CRAN.
#'
#' @examples
#' \dontrun{
#' loaded_pkgs()
#' is_installed(ggplot2)
#' is_installed(stats) == ("stats" %in% loaded_pkgs())
#' lib(downloader)
#' "downloader" %in% loaded_pkgs()
#' remove.packages("downloader")
#' in_cran(ggplot2)
#' in_cran("foo")
#' }
#'
#' @name pkg

#' @export
#' @rdname pkg
loaded_pkgs <- function() {
  s <- search()
  gsub("^package:", "", s[grepl("^package:", s)])
}

#' @export
#' @rdname pkg
is_installed <- function(pkg) {
  pkg_chr <- name_to_char(substitute(pkg))
  pkg_chr %in% utils::installed.packages()
} 

#' @export
#' @rdname pkg
lib <- function(pkg) {
  # TODO:
  # - Enhancement: pass character strings for development   [ ]
  #   packages (e.g. "githubusername/githubreponame") to
  #   devtools::install_github
  pkg_chr <- name_to_char(substitute(pkg))
  if (!is_installed(pkg_chr)) {
    utils::install.packages(pkg_chr)
  }
  invisible(library(package = pkg_chr,
                    character.only = TRUE,
                    logical.return = TRUE))
}

#' @export
#' @rdname pkg
in_cran <- function(pkg) {
  pkgs <- utils::available.packages()
  name_to_char(substitute(pkg)) %in% pkgs
}

# Helper function to coerce either a name or character string to a character
# string.
name_to_char <- function(x) {
  stopifnot(is.name(x) || is.character(x))
  ifelse(is.name(x), deparse(x), x)
}

