#' Plot cross sections on a plan view basemap
#'
#' Plots a select_xs object or a list_xs object on a plan view basemap. This
#' function only plots the cross section line segments. Assumes a plan view
#' basemap has already been plotted.
#'
#' @param xs A select_xs (see ?select_xs) or list_xs (see ?list_xs) object.
#' @param col Color(s) that the line used to indicate the specified cross section
#'   should be plotted. Can be a vector or an atomic value. Defaults to "blue".
#'
#' @return
#'
#' @keywords xs
#'
#' @export
#'
#' @examples
#' bm <-
#' list_xs(list(select_xs("A", plot = TRUE, basemap = bm, col = "red"),
#'              select_xs("B", col = "blue"), select_xs("C", col = "green")),
#'         plot = TRUE, basemap = bm, col = "red")
#' windows()
#' plot_xs(list_xs, col = "red")

plot_xs_plan <- function(xs, col = "blue") {
  ## xs should be a select_xs object or a list_xs object
  for (i in unique(xs[["xs_name"]])) {
    lines(xs[xs_name == i, list(x, y)], col = col)
    text(xs[xs_name == i, list(x, y)],
         labels = c(i, rep(NA, nrow(xs[xs_name == i]) - 2), paste0(i, "'")),
         pos = 3, col = col)
  }
}
