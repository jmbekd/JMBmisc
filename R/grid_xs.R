#' Set up the plotting region for the cross section
#'
#' This function sets up the plotting region for the cross section. Inputs are the total
#' length of the cross section, the depth below ground surface of the boreholes and/or wells
#' (or the maximum desired depth below ground surface that the cross section should extend),
#' and the ground surface elevation of the boreholes and/or wells. Using the supplied
#' information, and the cross section parameters set up using the xs_params function, an empty
#' gridded cross section will be plotted.
#'
#' @param data A data.frame or data.table with columns of "distance", "depth", and
#'   "gs_elev". For explanations of these parameters and their valid values, see below. The data
#'   parameter is not necessary if the distance, depth, and gs_elev parameters are individually
#'   specified. Defaults to NULL.
#' @param distance Horizontal distance of the boreholes and/or wells along the cross section or
#'   the maximum distance that the cross section should extend. Can be a single value or a
#'   vector of distances. If a vector of values is supplied, the maximum value is selected.
#'   Defaults to NULL.
#'   Note, the distance parameter is rounded to the nearest h_major unit (see ?xs_params) and
#'   the x-axis limits are determined using the 'pretty' function (see ?pretty) with an
#'   interval of 0 to the rounded distance value.
#' @param depth The total depth of the boreholes and/or wells below ground surface, or the
#'   the maximum distance below ground surface that the cross section should extend to. Can be
#'   a single value or a vector of depths. If a vector of depths is supplied, the maximum value
#'   is selected. Defaults to NULL.
#'   Note, the depth parameter is rounded to the nearest v_major unit (see ?xs_params)
#'   and the y-axis limits are determined using the 'pretty' function (see ?pretty) with
#'   the upper limit being the ground surface elevation (see gs_elev) and the lower limit
#'   being gs_elev - the rounded depth value.
#' @param gs_elev The ground surface elevation at each of the boreholes and/or wells along the
#'   cross section. Can be a single value or a vector of depths. If a vector of ground
#'   surface elevations is supplied, the maximum value is selected. Defaults to NULL.
#'   Note, the gs_elev parameter is rounded to the nearest v_minor unit (see ?xs_params)
#'   and the y-axis limits are determined using the 'pretty' function (see ?pretty) with
#'   the upper limit being the rounded gs_elev value and the lower limit
#'   being the rounded gs_elev - and the depth (see depth).
#' @param name The cross section ID (e.g., A, B, etc.). Defaults to NULL.
#' @param xs_par A list of cross section parameters from the xs_params function (see
#'   ?xs_params). Defaults to xs_params().
#'
#' @return A plot of an empty gridded cross section.
#'
#' @keywords xs, cross section, plot, gs_elev, depth, distance, xs_params, grid
#'
#' @export
#'
#' @examples
#' grid_xs(distance = c(100, 200, 600), depth = c(30, 15, 55), gs_elev = c(10, 12, 11),
#' name = "A", xs_par = xs_params())

grid_xs <- function(data = NULL,
                    distance = NULL, depth = NULL, gs_elev = NULL,
                    name = NULL, xs_par = xs_params()) {

  if (!is.null(data)) {
    names <- names(data)
    if (any(!(c("distance", "depth", "gs_elev") %in% names))) {
      stop ("please provide a data.frame with columns identified as 'distance', 'depth', and
            'gs_elev' or specify the 'distance', 'depth', and 'gs_elev' parameters directly.")
    }
    }

  if (is.null(data) & any(is.null(distance) & is.null(depth) & is.null(gs_elev))) {
    stop ("please provide a data.frame with columns identified as 'distance', 'depth', and
          'gs_elev' or specify the 'distance', 'depth', and 'gs_elev' parameters directly.")
  }

  if (length(distance) > 1) {
    distance <- max(distance)
  }
  if (length(depth) > 1) {
    depth <- max(depth)
  }
  if (length(gs_elev) > 1) {
    gs_elev <- max(gs_elev)
  }

  ## if distance, depth, and gs_elev parameters not specified, determine from data
  if (is.null(distance)) distance <- max(data[["distance"]])
  if (is.null(depth)) depth <- max(data[["depth"]])
  if (is.null(gs_elev)) gs_elev <- max(data[["gs_elev"]])

  ## round the distance, depth, and gs_elev parameters
  distance <- plyr::round_any(distance, xs_par[["h_major"]], f = ceiling)
  depth <- plyr::round_any(depth, xs_par[["v_major"]], f = ceiling)
  gs_elev <- plyr::round_any(gs_elev, xs_par[["v_minor"]], f = ceiling)

  ## determine the figure height and width and x and y axes limits
  y_lim <- range(pretty(c(gs_elev, gs_elev - depth)))
  x_lim <- range(pretty(c(0, distance)))
  fig_height <- diff(y_lim) / xs_par[["v_scale"]]
  fig_width <- diff(x_lim) / xs_par[["h_scale"]]

  par(pin = c(fig_width, fig_height), yaxs = "i", xaxs = "i",
      mar = c(5, 5, 5, 5)) # a value of 5 lines was chosen as 5 lines = 1 inch

  ## set up plotting region
  plot(0, 0, type = "n",
       xlim = x_lim, ylim = y_lim, axes = FALSE,
       xlab = paste0("Distance (" , xs_par[["units"]], ")"),
       ylab = paste0("Elevation (" , xs_par[["units"]], ")"),
       main = paste0("Cross Section ", name, "-", name, "'"))

  ## add major and minor tick marks to x and y axes
  axis(1, at = seq(from = x_lim[1], to = x_lim[2],
                   by = xs_par[["h_minor"]]), label = FALSE, tck = -0.01)
  axis(1, at = seq(from = x_lim[1], to = x_lim[2],
                   by = xs_par[["h_major"]]))
  axis(2, at = seq(from = y_lim[1], to = y_lim[2],
                   by = xs_par[["v_minor"]]), label = FALSE, tck = -0.01)
  axis(2, at = seq(from = y_lim[1], to = y_lim[2],
                   by = xs_par[["v_major"]]), las = 2)
  axis(4, at = seq(from = y_lim[1], to = y_lim[2],
                   by = xs_par[["v_minor"]]), label = FALSE, tck = -0.01)
  axis(4, at = seq(from = y_lim[1], to = y_lim[2],
                   by = xs_par[["v_major"]]), las = 2)
  mtext(text = paste0("Elevation (" , xs_par[["units"]], ")"), side = 4,
        line = 3)

  ## add major and minor gridlines to x and y axes
  if (xs_par[["plot_h_minor"]]) {
    abline(v = seq(from = x_lim[1], to = x_lim[2], by = xs_par[["h_minor"]]),
           lty = 3, lwd = 0.3)
  }
  if (xs_par[["plot_h_major"]]) {
    abline(v = seq(from = x_lim[1], to = x_lim[2], by = xs_par[["h_major"]]),
           lty = 3, lwd = 0.5)
  }
  if (xs_par[["plot_v_minor"]]) {
    abline(h = seq(from = y_lim[1], to = y_lim[2], by = xs_par[["v_minor"]]),
           lty = 3, lwd = 0.3)
  }
  if (xs_par[["plot_v_major"]]) {
    abline(h = seq(from = y_lim[1], to = y_lim[2], by = xs_par[["v_major"]]),
           lty = 2, lwd = 0.5)
  }
}
