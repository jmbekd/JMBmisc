#' Plot wells/boreholes on the cross section
#'
#' This function plots wells and/or boreholes on the cross section
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

plot_xs_locs <- function(data = null,
                         location = null, type = null, distance = null, gs_elev = null,
                         depth = null, tos = null, bos = null,
                         plot_gs = TRUE, w_width = 0.1, label_shift = 10,
                         cross_section_params = cross_section_params()) {

  ## data should be a data.frame or data.table with columns of "location", "type" (e.g., well,),
  ## borehole, etc.), "distance", "gs_elev" (ground surface elevation), "depth", "tos" (top of screen),
  ## and "bos" (bottom of screen). the data parameter is not necessary if the location, distance,
  ## gs_elev, depth, tos, and bos are supplied as separate vectors.
  ## w_width is the default width of the well on the output figure, in inches
  ## label_shift is the vertical distance to shift the well/borehole label above the top of the
  ## ground surface.

  if (!is.null(data)) {
    names <- names(data)
    if (any(!(c("location", "type", "distance", "gs_elev",
                "depth", "tos", "bos") %in% names))) {
      stop ("please provide a data.frame with columns identified as 'location', 'type',
            'distance', 'gs_elev', 'depth', 'tos', and 'bos' or specify the vectors of 'location',
            'type', 'distance', 'gs_elev', 'depth', 'tos', and 'bos' directly.")
    }
    }

  if (is.null(data) & any(is.null(location) & is.null(distance) & is.null(gs_elev) &
                          is.null(depth) & is.null(tos) & is.null(bos))) {
    stop ("please provide a data.frame or data.table with columns identified as 'location', 'type',
          'distance', 'gs_elev', 'depth', 'tos', and 'bos' or specify the vectors of 'location',
          'type', 'distance', 'gs_elev', 'depth', 'tos', and 'bos' directly.")
  }

  ## if the location, type, distance, gs_elev, depth, tos, and bos parameters not specified,
  ## determine their values from data
  if (is.null(location)) location <- data[["location"]]
  if (is.null(type)) location <- data[["type"]]
  if (is.null(distance)) distance <- data[["distance"]]
  if (is.null(gs_elev)) gs_elev <- data[["gs_elev"]]
  if (is.null(depth)) depth <- data[["depth"]]
  if (is.null(tos)) tos <- data[["tos"]]
  if (is.null(tos)) bos <- data[["bos"]]

  if (plot_gs) {
    lines(x = distance, y = gs_elev, lty = 1, lwd = 1.5)
  }

  ## plot a line from the ground surface to the bottom of the well/borehole
  segments(x0 = distance, y0 = gs_elev,
           x1 = distance, y1 = gs_elev - depth,
           lty = 1, lwd = 1, lend = "butt")

  ## plot a tick mark at the bottom of the well
  if (type == "well") {
    segments(x0 = distance - cross_section_params[["h_scale"]] * w_width / 2, y0 = gs_elev - depth,
             x1 = distance + cross_section_params[["h_scale"]] * w_width / 2, y1 = gs_elev - depth,
             lty = 1, lwd = 2, lend = "butt")
    ## plot a rectangle to the left of the well showing the screened interval
    rect(xleft = distance - cross_section_params[["h_scale"]] * w_width, ybottom = gs_elev - bos,
         xright = distance, ytop = gs_elev - tos, density = 24, angle = 0,
         col = "lightgrey")
  } else {
    points(distance, gs_elev - depth, pch = 19)
  }

  ## add location labels
  text(x = distance, y = elev + label_shift, labels = location, cex = 0.5, srt = 90, adj = c(0, 0.25))
  }