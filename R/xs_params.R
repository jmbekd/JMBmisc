#' Specify the cross section parameters
#'
#' This function allows you to specify desired cross section parameters such as
#' horizontal and vertical scales and gridlines.
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

xs_params <- function(v_scale = 10,
                                 h_scale = 100,
                                 v_minor = 1,
                                 v_major = 10,
                                 h_minor = 20,
                                 h_major = 100,
                                 units = "ft",
                                 plot_v_minor = TRUE,
                                 plot_v_major = TRUE,
                                 plot_h_minor = FALSE,
                                 plot_h_major = TRUE) {
  ## Sets up basic cross section parameters
  xs_params <- list("v_scale" = v_scale, "h_scale" = h_scale, "v_minor" = v_minor,
                    "v_major" = v_major, "h_minor" = h_minor, "h_major" = h_major,
                    "units" = units, "plot_v_minor" = plot_v_minor, "plot_v_major" = plot_v_major,
                    "plot_h_minor" = plot_h_minor, "plot_h_major" = plot_h_major)
}