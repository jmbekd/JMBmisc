#' Calculates theta, the angle of rotation, for a particular cross section segment.
#'
#' Calculates the angle of rotation, theta, for a cross section segment.
#'
#' @param xs A select_xs (see ?select_xs) or list_xs (see ?list_xs) object.
#'
#' @return A data table with a column entitled "theta"
#'
#' @keywords theta
#'
#' @export
#'
#' @examples
#' bm <-
#' xs <- list_xs(list(select_xs("A", plot = TRUE, basemap = bm, col = "red"),
#'              select_xs("B", col = "blue"), select_xs("C", col = "green")),
#'         plot = TRUE, basemap = bm, col = "red")
#' calc_theta(xs)



calc_theta <- function(xs) {
  # xs should be a select_xs object or a list_xs object
  xs[, list("theta" = atan(diff(y) / diff(x))), by = xs_name]
}
