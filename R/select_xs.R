#' Use a mouse to specify the desired cross section
#'
#' This function allows the user to specify the start, pivot, and end points
#' of the cross section by clicking the mouse pointer on locations shown on
#' a plan view basemap of the Site. After selecting the desired points, the
#' function will plot the selected cross section on a plan view basemap of the
#' Site.
#' This function returns a data.table with the name and the x and y coordinates
#' of the specified cross section.
#'
#' @param name The name of the cross section.
#' @param plot Should a the plan view basemap be plotted? Defaults to FALSE.
#' @param basemap A data.frame or data.table containing x and y coordinates that
#'   can be used to draw the basemap (see ?plot_bm)
#' @param col Color that the line used to indicate the specified cross section
#'   should be plotted. Defaults to "blue".
#'
#' @return A data.table with the name and the x and y coordinates of the specified
#'   cross section.
#'
#' @keywords xs
#'
#' @export
#'
#' @examples
#' bm <-
#' select_xs("A", plot = TRUE, basemap = bm, coll = "red")


select_xs <- function(name, plot = FALSE, basemap = NULL, col = "blue") {
  if ((plot) & (is.null(basemap))) stop("please supply the desired basemap.")
  if ((plot) & (!is.null(basemap))) plot_bm(bm)

  cat("click on the desired points along the cross section. click escape to exit.\n")
  xs <- data.table::as.data.table(locator())
  xs[, xs_name := name]
  xs[, segment := 1:nrow(xs) - 1]
  lines(xs[, list(x, y)], col = col)
  text(xs[, list(x, y)], labels = c(name, rep(NA, nrow(xs) - 2), paste0(name, "'")), pos = 3, col = col)
  return(xs)
}
