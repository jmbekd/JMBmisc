#' Plot wells/boreholes on the cross section
#'
#' This function plots wells and/or boreholes on the cross section. The function will plot a
#' rectangular screened interval for wells. The bottom of a well is denoted with a dash and
#' the bottom of a borehole is denoted with a point.
#'
#' @param dt A data.frame or data.table with columns of "location", "type", "distance",
#'   "gs_elev", "depth", "tos", and "bos". For explanations of these parameters and their
#'   valid values, see the definitions below. The data parameter is not necessary if the
#'   location, type, distance, gs_elev, depth, tos, and bos parameters are individually
#'   specified. Defaults to NULL.
#' @param location A unique identifier for each borehole and/or well (e.g., "MW-3", "BH-45",
#'   etc.). Should be a vector of character strings. Defaults to NULL.
#' @param type Is the specified location a well, borehole, etc.? Should be a vector of character
#'   strings such as "well", "borehole", "ssp", "svp", etc. Defaults to NULL.
#' @param distance Horizontal distance of each of the boreholes and/or wells along the cross
#'   section. Should be a vector of depths. Defaults to NULL.
#' @param gs_elev The ground surface elevation at each of the boreholes and/or wells along the
#'   cross section. Should be a vector of depths. Defaults to NULL.
#' @param depth The total depth (below ground surface) at each of the boreholes and/or wells
#'   along the cross section. Should be a vector of depths. Defaults to NULL.
#' @param tos The distance from the ground surface to the top of the screened interval for
#'   each well along the cross section. Should be a vector of depths. Defaults to NULL.
#' @param bos The distance from the ground surface to the tbottom of the screened interval for
#'   each well along the cross section. Should be a vector of depths. Defaults to NULL.
#' @param plot_gs Should a line be plotted between the ground surface elevations (gs_elev)
#'   of the boreholes and wells? Defaults to TRUE.
#' @param w_width Width of the well screen in inches. Defaults to 0.1.
#' @param label_shift The vertical distance by which to shift the borehole and/or well
#'   labels. Defaults to 2.
#' @param xs_par A list of cross section parameters from the xs_params function (see
#'   ?xs_params). Defaults to xs_params().
#'
#' @return Plots the wells and boreholes on the cross section set up using grid_xs().
#'
#' @keywords xs, plot, cross section
#'
#' @export
#'
#' @examples
#'
# grid_xs(distance = c(100, 200, 550),
#         depth = c(30, 15, 55),
#         gs_elev = c(10, 12, 11),
#         name = "A", xs_par = xs_params())
# plot_xs_locs(location = c("MW-1", "BH-5", "CPT-3"),
#              type = c("well", "borehole", "cpt"),
#              distance = c(100, 200, 550),
#              gs_elev = c(11, 8, 12),
#              depth = c(60, 42, 90),
#              tos = c(45, NA, NA),
#              bos = c(55, NA, NA))

plot_xs_locs <- function(dt = NULL,
                         location = NULL, type = NULL, distance = NULL, gs_elev = NULL,
                         depth = NULL, tos = NULL, bos = NULL,
                         plot_gs = TRUE, w_width = 0.1, label_shift = 2,
                         xs_par = xs_params()) {

  ## data should be a data.frame or data.table with columns of "location", "type" (e.g., well,),
  ## borehole, etc.), "distance", "gs_elev" (ground surface elevation), "depth", "tos" (top of screen),
  ## and "bos" (bottom of screen). the data parameter is not necessary if the location, distance,
  ## gs_elev, depth, tos, and bos are supplied as separate vectors.
  ## w_width is the default width of the well on the output figure, in inches
  ## label_shift is the vertical distance to shift the well/borehole label above the top of the
  ## ground surface.

  if (!is.null(dt)) {
    names <- names(dt)
    if (any(!(c("location", "type", "distance", "gs_elev",
                "depth", "tos", "bos") %in% names))) {
      stop ("please provide a data.frame with columns identified as 'location', 'type',
            'distance', 'gs_elev', 'depth', 'tos', and 'bos' or specify the vectors of 'location',
            'type', 'distance', 'gs_elev', 'depth', 'tos', and 'bos' directly.")
    }
    location <- dt[["location"]]
    type <- dt[["type"]]
    distance <- dt[["distance"]]
    gs_elev <- dt[["gs_elev"]]
    depth <- dt[["depth"]]
    tos <- dt[["tos"]]
    bos <- dt[["bos"]]
  }

  if (is.null(dt) & any(is.null(location) & is.null(distance) & is.null(gs_elev) &
                          is.null(depth) & is.null(tos) & is.null(bos))) {
    stop ("please provide a data.frame or data.table with columns identified as 'location', 'type',
          'distance', 'gs_elev', 'depth', 'tos', and 'bos' or specify the vectors of 'location',
          'type', 'distance', 'gs_elev', 'depth', 'tos', and 'bos' directly.")
  }

  if (plot_gs) {
    lines(x = distance,
          y = gs_elev,
          lty = 1, lwd = 1.5)
  }

  ## plot a line from the ground surface to the bottom of the well/borehole
  segments(x0 = distance,
           y0 = gs_elev,
           x1 = distance,
           y1 = gs_elev - depth,
           lty = 1, lwd = 1, lend = "butt")

  wells <- tolower(type) == "well"

  ## plot a tick mark at the bottom of the well
  segments(x0 = distance[wells] - xs_par[["h_scale"]] * w_width / 2,
           y0 = gs_elev[wells] - depth[wells],
           x1 = distance[wells] + xs_par[["h_scale"]] * w_width / 2,
           y1 = gs_elev[wells] - depth[wells],
           lty = 1, lwd = 2, lend = "butt")

  ## plot a rectangle to the left of the well showing the screened interval
  rect(xleft = distance[wells] - xs_par[["h_scale"]] * w_width,
       ybottom = gs_elev[wells] - bos[wells],
       xright = distance[wells],
       ytop = gs_elev[wells] - tos[wells],
       density = 24, angle = 0)

  ## plot a point at the total depth of each non-well borehole
  points(x = distance[!wells],
         y = gs_elev[!wells] - depth[!wells],
         pch = 19)

  ## add location labels
  text(x = distance,
       y = gs_elev + label_shift,
       labels = location,
       cex = 0.5, srt = 90, adj = c(0, 0.25), xpd = TRUE)
}
