#' 2D rotation about an arbitrary point in space
#'
#' Rotates a x and y coordinates by theta (the angle of rotation) about an
#' arbitrary point in space. Rotates the coordinates clockwise towards the
#' horizontal positive x-axis.
#'
#' @param coords A data.frame or data.table of x and y values (in feet; this
#'   function will not work for latitude and longitude values that are in degrees).
#' @param origin The x and y value around which coords will be rotated. Defaults
#'   to c(0, 0).
#' @param theta The angle of rotation through which the x and y values should be
#'   rotated. The angle should be in radians or degrees.
#' @param units The units for theta. Should be radians ("rad") or degrees ("deg").
#'   Defaults to "rad".
#'
#' @return A data.frame with the rotated x and y coordinates
#'
#' @keywords rotate, theta
#'
#' @export
#'
#' @examples
#' df <- data.frame("x" = 1:5, "y" = 1:5)
#' rotate(df, theta = pi / 4)
#' rotate(df, origin = c(5, 5), theta = 90, units = "deg")


rotate <- function(coords, origin = c(0, 0), theta, units = "rad") {
  theta <- -theta
  if (units == "deg") theta <- theta * pi / 180
  R <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)),
              nrow = 2, ncol = 2, byrow = TRUE)
  r_data <- t(R %*% t(coords - as.list(origin)))
  as.data.frame(r_data)
}
