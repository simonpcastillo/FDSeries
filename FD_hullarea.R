#' Internal use
#' @description Internal use. It computes the convex hull for a set of coordinates.
#' @param
#'    x: numeric vector of coordinates.
#'    y: numeric vector of coordinates.
#'    x and y must have the same length.
#' @return It returns a data.frame with the vertices of the convex hull'



hullarea <- function (x,y) {
  ne <- length(x)
  harea <- abs (0.5 * ( (x[1:(ne-1)] %*% y[2:ne]) - ( y[1:(ne-1)] %*% x[2:ne]) ) )
  harea
}
