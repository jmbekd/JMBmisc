#' Plot a plan view of the basemap
#'
#' This function plots a plan view of the basemap
#'
#' @param basemap A data.frame or data.table containing x and y coordinates that
#'   can be used to draw the basemap.
#'
#' @return
#'
#' @keywords bm, basemap
#'
#' @export
#'
#' @examples
#'
# plot_bm(bm)

plot_bm <- function(basemap) {
  plot(basemap[, list(x, y)], type = "n",
       xlab = "Easting", ylab = "Northing", main = "Basemap", asp = 1)
  lines(basemap[, list(x, y)])
}
