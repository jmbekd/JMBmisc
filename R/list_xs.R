#' Create a list of user specified cross sections
#'
#' This creates takes a list of cross section objects developed using select_xs
#' and binds them together into a data.table. Will plot the cross sections
#' on a plan view basemap if desired.
#'
#' @param xs_list A list of cross sections objects developed using select_xs
#'   (see ?select_xs)
#' @param plot Should a the plan view basemap be plotted? Defaults to FALSE.
#' @param basemap A data.frame or data.table containing x and y coordinates that
#'   can be used to draw the basemap (see ?plot_bm)
#' @param col Color(s) that the line used to indicate the specified cross section
#'   should be plotted. Can be a vector or an atomic value. Defaults to "blue".
#'
#' @return A data.table of the cross sections and their start, pivot, and end
#'   points.
#'
#' @keywords
#'
#' @export
#'
#' @examples
#'
#' list_xs(list(select_xs("A", plot = TRUE, basemap = bm, col = "red"),
#'              select_xs("B", col = "blue"), select_xs("C", col = "green")),
#'         plot = TRUE, basemap = bm, col = "red")

list_xs <- function(list_of_xs, plot = FALSE, basemap = NULL, col = "blue") {
  xs <- data.table::rbindlist(list_of_xs)
  if ((plot) & (is.null(basemap))) stop("please supply the desired basemap.")
  if ((plot) & (!is.null(basemap))) {
    plot_bm(basemap)
    plot_xs(xs, col = col)
  }
  return(xs)
}
