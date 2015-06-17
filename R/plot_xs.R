#' Set up the plotting region for the cross section
#'
#' This function sets up the plotting region for the cross section
#'
#' @param v_scale Vertical scale of the y-axis. Defaults to 10 (i.e.,
#'   in the resulting plot, 10 feet on the y-axis will correspond to 1 inch printed).
#' @param h_scale Horizontal scale of the x-axis. Defaults to 100 (i.e.,
#'   in the resulting plot, 100 feet on the x-axis will correspond to 1 inch printed).
#' @param v_minor Interval for plotting minor horizontal grid lines. Defaults to 1 (i.e.,
#'   a minor gridline every foot).
#' @param v_major Interval for plotting major horizontal grid lines. Defaults to 10 (i.e.,
#'   a major gridline every 10 feet).
#' @param h_minor Interval for plotting minor vertical grid lines. Defaults to 20 (i.e.,
#'   a minor gridline every 20 feet).
#' @param h_major Interval for plotting major vertical grid lines. Defaults to 100 (i.e.,
#'   a major gridline every 100 feet).
#' @param h_major Interval for plotting major vertical grid lines. Defaults to 100 (i.e.,
#'   a major gridline every 100 feet).
#' @param units Unit of measurement for the x and y axes. Defaults to feet ("ft").
#' @param plot_v_minor Plot minor horizontal grid lines? Defaults to TRUE.
#' @param plot_v_major Plot major horizontal grid lines? Defaults to TRUE.
#' @param plot_h_minor Plot minor vertical grid lines? Defaults to FALSE.
#' @param plot_h_major Plot major vertical grid lines? Defaults to TRUE.
#'
#' @return List of parameters that will be used to set desired cross section parameters.
#'
#' @keywords xs, params, cross section
#'
#' @export
#'
#' @examples
#' xs_params()

plot_xs <- function(data = null,
                               distance = null, depth = null, gs_elev = null,
                               cross_section_params = cross_section_params()) {
  ## data should be a data.frame or data.table with columns of
  ## "distance", "depth" (total depth), and "gs_elev" (ground surface elevation).
  ## the data parameter is not necessary if the dist, depth, and gs_elev parameters are
  ## individually specified.

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
    warning("more than one distance specified. only the first entry will be used.")
    distance <- distance[1]
  }
  if (length(depth) > 1) {
    warning("more than one distance specified. only the first entry will be used.")
    depth <- depth[1]
  }
  if (length(gs_elev) > 1) {
    warning("more than one distance specified. only the first entry will be used.")
    gs_elev <- gs_elev[1]
  }

  ## if distance, depth, and gs_elev parameters not specified, determine from data
  if (is.null(distance)) distance <- max(data[["distance"]])
  if (is.null(depth)) depth <- max(data[["depth"]])
  if (is.null(gs_elev)) gs_elev <- max(data[["gs_elev"]])

  ## round the distance, depth, and gs_elev parameters
  distance <- plyr::round_any(distance, cross_section_params[["h_major"]], f = ceiling)
  depth <- plyr::round_any(depth, cross_section_params[["v_major"]], f = ceiling)
  gs_elev <- plyr::round_any(gs_elev, cross_section_params[["v_minor"]], f = ceiling)

  ## determine the figure height and width and x and y axes limits
  fig_height <- depth / cross_section_params[["v_scale"]]
  fig_width <- distance / cross_section_params[["h_scale"]]
  y_lim <- range(pretty(c(gs_elev, gs_elev - depth)))
  x_lim <- range(pretty(c(0, dist)))

  ## set up plotting region
  plot(0, 0, type = "n",
       xlim = x_lim, ylim = y_lim, axes = FALSE,
       xlab = paste0("Distance (" , cross_section_params[["units"]], ")"),
       ylab = paste0("Elevation (" , cross_section_params[["units"]], ")"),
       main = paste0("Cross Section ", name, "-", name, "'"))

  ## add major and minor tick marks to x and y axes
  axis(1, at = seq(from = x_lim[1], to = x_lim[2],
                   by = cross_section_params[["h_minor"]]), label = FALSE, tck = -0.025)
  axis(1, at = seq(from = x_lim[1], to = x_lim[2],
                   by = cross_section_params[["h_major"]]))
  axis(2, at = seq(from = y_lim[1], to = y_lim[2],
                   by = cross_section_params[["v_minor"]]), label = FALSE, tck = -0.025)
  axis(2, at = seq(from = y_lim[1], to = y_lim[2],
                   by = cross_section_params[["v_major"]]), las = 2)
  axis(4, at = seq(from = y_lim[1], to = y_lim[2],
                   by = cross_section_params[["v_minor"]]), label = FALSE, tck = -0.025)
  axis(4, at = seq(from = y_lim[1], to = y_lim[2],
                   by = cross_section_params[["v_major"]]), las = 2)

  ## add major and minor gridlines to x and y axes
  if (cross_section_params[["plot_h_minor"]]) {
    abline(v = seq(from = x_lim[1], to = x_lim[2], by = h_minor), lty = 3, lwd = 0.3)
  }
  if (cross_section_params[["plot_h_major"]]) {
    abline(v = seq(from = x_lim[1], to = x_lim[2], by = h_major), lty = 3, lwd = 0.5)
  }
  if (cross_section_params[["plot_v_minor"]]) {
    abline(h = seq(from = y_lim[1], to = y_lim[2], by = v_minor), lty = 3, lwd = 0.3)
  }
  if (cross_section_params[["plot_v_major"]])
    abline(h = seq(from = y_lim[1], to = y_lim[2], by = v_major), lty = 2, lwd = 0.5)
  }