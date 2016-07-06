#' Calculate distance between geographic coordinate pairs
#' 
#' Calculate distances between sets of latitude and longitude coordinates using
#' the law of haversines.  Rough approximation that assumes a spherical Earth. 
#'
#' @param l1 a n x 2 matrix of geographic coordinates.  Each row represents a
#' point with latitude in the first column and longitude in the second column.
#' @param l2 a matrix with a schema identical to l1.
#' @param r a number for the radius of the reference sphere.  Default is the
#' average radius of Earth in kilometers
#' (\url{https://www.wolframalpha.com/input/?i=radius+of+earth})
#' @return a vector of length n where each element is the distance between a
#' point in l1 and its corresponding point in l2. 
#'
#' @examples
#' l1 <- matrix(runif(20), 20, 2)
#' l2 <- matrix(runif(20), 20, 2)
#' haversine_dist(l1, l2)
#'
#' @references \url{https://en.wikipedia.org/wiki/Haversine_formula}
#' @export
haversine_dist <- function(l1, l2, r = 6367.4447) {
  stopifnot(is.numeric(l1), is.numeric(l2), ndim(l1) == 2, ndim(l2) == 2)
  l1 <- deg2rad(l1)
  l2 <- deg2rad(l2)
  a <- 0.5 * (1 - cos(l1[, 1] - l2[, 1]))  
  b <- cos(l1[, 1]) * cos(l2[, 1])
  c <- 0.5 * (1 - cos(l1[, 2] - l2[, 2]))
  stopifnot((a + b * c) < 1, (a + b * c) > 0)
  2 * r * asin(sqrt(a + b * c))
}

rad2deg <- function(rad) {
  (rad * 180) / pi
}

deg2rad <- function(deg) {
  (deg * pi) / 180
}

