#### Functions to plot a plan view basemap, select cross sections, plot cross sections on the plan view basemap, and
#### create a list_xs object (a data.table summarizing the names, pivot points, and coordinats of the selected
#### cross sections) ----

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

list_xs <- function(list_of_xs, plot = FALSE, basemap = NULL, col = "blue") {
  ## list_of_xs is a list of the cross sections developed using select_xs
  xs <- data.table::rbindlist(list_of_xs)
  if ((plot) & (is.null(basemap))) stop("please supply the desired basemap.")
  if ((plot) & (!is.null(basemap))) {
    plot_bm(basemap)
    plot_xs(xs, col = col)
  }
  return(xs)
}

plot_bm <- function(basemap) {
  plot(basemap[, list(x, y)], type = "n", xlab = "Easting", ylab = "Northing", main = "Basemap", asp = 1)
  lines(basemap[, list(x, y)])
}

plot_xs <- function(xs, col = "blue") {
  ## xs should be a select_xs object or a list_xs object
  for (i in unique(xs[["xs_name"]])) {
    lines(xs[xs_name == i, list(x, y)], col = col)
    text(xs[xs_name == i, list(x, y)],
         labels = c(i, rep(NA, nrow(xs[xs_name == i]) - 2), paste0(i, "'")),
         pos = 3, col = col)
  }
}


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


calc_theta <- function(xs) {
  # xs should be a select_xs object or a list_xs object
  xs[, list("theta" = atan(diff(y) / diff(x))), by = xs_name]
}


xs_lines <- function(xs) {
  # xs should be a select_xs object or a list_xs object
  lines <- cbind(xs[, .SD[1:.N-1], by = xs_name],
                 xs[, list("theta" = atan(diff(y) / diff(x)),
                           "slope" = diff(y) / diff(x)), by = xs_name][, list(theta, slope)])
  lines[, intercept := y - slope * x]
}

offset <- 200

lines <- cbind(xs[, .SD[1:.N-1], by = xs_name],
               xs[, list("theta" = ifelse(diff(x) < 0, atan(diff(y) / diff(x)) + pi, atan(diff(y) / diff(x)))),
                  by = xs_name][, list(theta)],
               xs[, list("slope" = diff(y) / diff(x)), by = xs_name][, list(slope)])
lines[, c("x_shift", "y_shift") := list(offset * sin(theta), offset * cos(theta))]

pt <- data.frame(locator())

(pt[["x"]] - lines[, x] + lines[, .SD * list(1, -1), .SDcols = c("x_offset", "x_offset")]) /
  (pt[["y"]] - lines[, y] + lines[, .SD * list(1, -1), .SDcols = c("y_offset", "y_offset")])

lines[, .SD * list(1, -1), .SDcols = c("x_offset", "x_offset")]


xs[, list("theta" = atan(diff(y) / diff(x))), by = xs_name][, offset := offset / cos(theta)]

calc_theta(select_xs("1st", plot = TRUE, basemap = bm))
calc_theta(select_xs("2nd"))
calc_theta(select_xs("3rd", col = "red"))
calc_theta(select_xs("4th", col = "red"))



#### Functions to plot cross sections, well screens, chemistry data labels, and cpt/mip data ----


## Plot chemistry results
plot_xs_chem_labels <- function(dist, elev, depth, tos, bos, id, chem) {
  w_width <- 0.1 # default width of well, in inches
  rect(xleft = dist - h_exag * w_width, ybottom = elev - bos,
       xright = dist, ytop = elev - tos, border = NA,
       col = "lightgrey")
  labs <- paste0("PCE=", chem[["PCE"]], "\n",
                 "TCE=", chem[["TCE"]], "\n",
                 "cDCE=", chem[["cDCE"]], "\n",
                 "tDCE=", chem[["tDCE"]], "\n",
                 "VC=", chem[["VC"]])
  text(dist, elev - (tos + bos) / 2, labels = labs[1:3], pos = 2, cex = 0.5)
}



## cross section parameters from user input
v_exag <- 8
h_exag <- 100
v_minor <- 1
v_major <- 10
h_minor <- 20
h_major <- 100
units <- "ft"
title <- "Cross Section Name"


## cross section parameters from data
dist <- 1600
depth <- 80
gs_elev <- 12



fig_height <- depth / v_exag
fig_width <- dist / h_exag
y_lim <- range(pretty(c(gs_elev, -depth)))
x_lim <- range(pretty(c(0, dist)))

plot(0, 0, type = "n",
     xlim = x_lim, ylim = y_lim, axes = FALSE,
     xlab = paste0("Distance (" , units, ")"),
     ylab = paste0("Elevation (" , units, ")"),
     main = title)
axis(1, at = seq(from = x_lim[1], to = x_lim[2], by = h_minor), label = FALSE, tck = -0.025)
axis(1, at = seq(from = x_lim[1], to = x_lim[2], by = h_major))
axis(2, at = seq(from = y_lim[1], to = y_lim[2], by = v_minor), label = FALSE, tck = -0.025)
axis(2, at = seq(from = y_lim[1], to = y_lim[2], by = v_major), las = 2)

# abline(v = seq(from = x_lim[1], to = x_lim[2], by = h_minor), lty = 3, lwd = 0.3)
abline(v = seq(from = x_lim[1], to = x_lim[2], by = h_major), lty = 3, lwd = 0.5)
abline(h = seq(from = y_lim[1], to = y_lim[2], by = v_minor), lty = 3, lwd = 0.3)
abline(h = seq(from = y_lim[1], to = y_lim[2], by = v_major), lty = 2, lwd = 0.5)

loc <- c("W1", "W2", "W3")
w_dist <- c(200, 600, 1300)
w_gs_elev <- c(11, 8, 13)
w_depth <- c(60, 80, 35)
tos <- c(45, 25, 9)
bos <- c(55, 30, 14)

wells <- data.table(loc, w_dist, w_gs_elev, w_depth, tos, bos)

plot_xs_wells <- function(dist, elev, depth, tos, bos, id) {
  w_width <- 0.1 # default width of well, in inches
  ## plot ground surface elevation
  lines(x = dist, y = elev, lty = 1, lwd = 1.5)
  segments(x0 = dist, y0 = elev,
           x1 = dist, y1 = elev - depth,
           lty = 1, lwd = 1, lend = "butt")
  segments(x0 = dist - h_exag * w_width / 2, y0 = elev - depth,
           x1 = dist + h_exag * w_width / 2, y1 = elev - depth,
           lty = 1, lwd = 2, lend = "butt")
  rect(xleft = dist - h_exag * w_width, ybottom = elev - bos,
       xright = dist, ytop = elev - tos, density = 24, angle = 0,
       col = "black")
}


