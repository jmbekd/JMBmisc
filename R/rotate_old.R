#### Functions to plot a plan view basemap, select cross sections, plot cross sections on the plan view basemap, and
#### create a list_xs object (a data.table summarizing the names, pivot points, and coordinats of the selected
#### cross sections) ----






#### Functions to subset, rotate, and calculate distances along each cross section ----

rotate <- function(coords, origin = c(0, 0), theta, units = "rad") {
  ## coords should be a data.frame of x and y values (in feet; this function will not work
  ## for latitude and longitude values that are in degrees) that has been centered around
  ## the origin of rotation
  ##
  ## origin should be the x and y values (in feet) of the point around which the rotation should occur
  ##
  ## theta is the angle between the cross section and the positive x-axis, theta should be
  ## measured in a counter clockwise manner

  theta <- -theta # this will rotate the coordinates clockwise to the horizontal positive x-axis
  if (units == "deg") theta <- theta * pi / 180
  R <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)),
              nrow = 2, ncol = 2, byrow = TRUE)
  r_data <- t(R %*% t(coords - as.list(origin)))
  as.data.frame(r_data)
}


#
# xs_lines <- function(xs) {
#   # xs should be a select_xs object or a list_xs object
#   lines <- cbind(xs[, .SD[1:.N-1], by = xs_name],
#                  xs[, list("theta" = atan(diff(y) / diff(x)),
#                            "slope" = diff(y) / diff(x)), by = xs_name][, list(theta, slope)])
#   lines[, intercept := y - slope * x]
# }
#
# offset <- 200
#
# lines <- cbind(xs[, .SD[1:.N-1], by = xs_name],
#                xs[, list("theta" = ifelse(diff(x) < 0, atan(diff(y) / diff(x)) + pi, atan(diff(y) / diff(x)))),
#                   by = xs_name][, list(theta)],
#                xs[, list("slope" = diff(y) / diff(x)), by = xs_name][, list(slope)])
# lines[, c("x_shift", "y_shift") := list(offset * sin(theta), offset * cos(theta))]
#
# pt <- data.frame(locator())
#
# (pt[["x"]] - lines[, x] + lines[, .SD * list(1, -1), .SDcols = c("x_offset", "x_offset")]) /
#   (pt[["y"]] - lines[, y] + lines[, .SD * list(1, -1), .SDcols = c("y_offset", "y_offset")])
#
# lines[, .SD * list(1, -1), .SDcols = c("x_offset", "x_offset")]
#
#
# xs[, list("theta" = atan(diff(y) / diff(x))), by = xs_name][, offset := offset / cos(theta)]
#
# calc_theta(select_xs("1st", plot = TRUE, basemap = bm))
# calc_theta(select_xs("2nd"))
# calc_theta(select_xs("3rd", col = "red"))
# calc_theta(select_xs("4th", col = "red"))
#
#
#
#
