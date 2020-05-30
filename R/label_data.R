library("data.table")
distance <- c(1, 1.1, 1.25)
gs_elev <- c( 2, 2.1, 3)
labels <- c("one", "eleven", "George st")

loc_labels_orig <- data.table(x = distance, y = gs_elev, label = labels)

plot(1:5, 1:5, type = "n")
label_data <- function(data,
                       # label = TRUE,
                       # n_iter = 50,
                       # tolerance = 0.01,
                       ...) {

  data <- unique(data)
  setorder(data, x, y) # Order data
  data[, c("xo", "yo") := .(x, y)] # Save the original x, y coordinates
  data[, label := paste0(" ", label, " ")] # Adds some additional space for bounding box

  #### Calculates length and width of labels ----
  cex_val <- ifelse(c("cex") %in% names(list(...)),
                    list(...)[["cex"]],
                    par("cex"))
  data[, width_str :=
         round(strwidth(label,
                        cex = cex_val),
               6)]
  data[, height_str :=
         round(strheight(label,
                         cex = cex_val * 5 / 3),
               6)] # Adds some padding along height
  #### End calculation of length and width of labels ----
  bb_labels(data, ...)
  # data[, bb_labels(data, ...)]
  cat("This is the current 'data'\n")
  print(data[])
  cat("\n")
# iter <- 1
# while (iter < n_iter) {
#   # Calculate bounding boxes for labels
#   data[, label_bounding_box(data, ...)]
#   print(data)
#   # Determine if there is overlap greater than the tolerance level
#   data[, overlap := (xleft - shift(xright, 1, type = "lead") > -1 * tolerance) &
#          (xright - shift(xleft, 1, type = "lead") > tolerance) &
#          (ytop - shift(ybottom, 1, type = "lead") > tolerance) &
#          (ybottom - shift(ytop, 1, type = "lead") < -1 * tolerance)]
#   if (!(any(data[["overlap"]], na.rm = TRUE))) {iter <- n_iter}
#
#   # Shift labels if there is overlap
#   # data[, shift_labels(data, iteration = iter)]
#   iter <- iter + 1
# }
#
# #### Plots labels ----
  # For the labeling to work, require 'pos = 4' and 'offset = 0'
  # If 'srt = 0', the 'x' coordinate is left most coordinate of the
  # text bounding box (i.e., x0) and the 'y' coordinate is the vertical
  # midpoint text bounding box (i.e., (y0 + y1) / 2).
  # If 'srt = 90', the 'x' coordinate is the horizontal midpoint of the
  # text bounding box (i.e., (x0 + x1) / 2) and the 'y' coordinate is
  # the bottom most coordinate of the bounding box (i.e., y0)
  data[, text(x = x,
              y = y,
              label = label,
              pos = 4,
              offset = 0,
              ...)]
# #### Plots rectangles around text - Used for error checking ----
  data[, rect(xleft,
              ybottom,
              xright,
              ytop,
              border = .I), by = .I]
return(data[])
}

bb_labels <- function(data, ...) { # Calculates bounding boxes for text labels
  plot_usr <- par("usr")
  data[, xleft := x]
  data[, xright := x + width_str]
  data[, ybottom := y - height_str / 2]
  data[, ytop := y + height_str / 2]

  if (c("srt") %in% names(list(...))) {
    if (list(...)[["srt"]] == 90) {
      data[, temp := width_str]
      data[, width_str := height_str / (plot_usr[4] - plot_usr[3]) * par("pin")[2]]
      data[, width_str := width_str / par("pin")[1] * (plot_usr[2] - plot_usr[1])]
      data[, height_str := temp / (plot_usr[2] - plot_usr[1]) * par("pin")[1]]
      data[, height_str := height_str / par("pin")[2] * (plot_usr[4] - plot_usr[3])]
      data[, temp := NULL] # removes temporary column
      data[, width_str := round(width_str, 6)] # forces rounding to 6 sig figs
      data[, height_str := round(height_str, 6)] # forces rounding to 6 sig figs

      data[, xleft := x - width_str / 2]
      data[, xright := x + width_str / 2]
      data[, ybottom := y]
      data[, ytop := y + height_str]
    }
  }
  return(data)
}

shift_labels <- function(data, iteration) {
  for (a in which(data[["overlap"]] == TRUE)) { # loops through all entries which overlap
    iteration <- iteration

    x_overlap <- c(data[a, xright] - data[a + 1, xleft],
                   data[a, xleft] - data[a + 1, xright])
    x_overlap <- x_overlap[which(abs(x_overlap) == min(abs(x_overlap)))] # selects the minimum overlap
    x_overlap <- round(x_overlap, 6) # forces rounding to 6 sig figs
    y_overlap <- c(data[a, ytop] - data[a + 1, ybottom],
                   data[a, ybottom] - data[a + 1, ytop])
    y_overlap <- y_overlap[which(abs(y_overlap) == min(abs(y_overlap)))] # selects the minimum overlap
    y_overlap <- round(y_overlap, 6) # forces rounding to 6 sig figs

    set(data, # adds the amount of overlap along the x axis to the data table
        i = a,
        j = "overlap_x",
        value = x_overlap)
    set(data, # adds the amount of overlap along the y axis to the data table
        i = a,
        j = "overlap_y",
        value = y_overlap)
    set(data, # records iteration
        i = a,
        j = "iteration",
        value = iteration)

    overlap_amt <- data.frame("x" = x_overlap, "y" = y_overlap)
    column <- names(overlap_amt)[which(abs(overlap_amt) == min(abs(overlap_amt)))] # selects the axis with the minimum overlap
    shift <- round(overlap_amt[, column] / 2, 6) # calculates the label shift, forces 6 sig figs
    new_values <- data[a:(a + 1), .SD + c(-1, 1) * shift, .SDcols = c(column)]
    set(data, # replaces the old values
        i = a:(a + 1),
        j = column,
        value = new_values)
  }
  return(data[])
}





# add.alpha function from https://gist.github.com/mages/5339689
add.alpha <- function(cols, alpha) rgb(t(col2rgb(cols) / 255),
                                       alpha = alpha)

#from TeachingDemos package
shadowtext <- function(x, y = NULL, labels,
                       col = "white", bg = "black",
                       theta = seq(pi / 8, 2 * pi, length.out = 16),
                       r = 0.01, ... ) {
  ## From TeachingDemos package

  xy <- xy.coords(x, y)
  xo <- r * strwidth("A")
  yo <- r * strheight("A")

  for (i in theta) {
    text(xy$x + cos(i) * xo, xy$y + sin(i) * yo,
         labels, col = bg, ... )
  }

  text(xy$x, xy$y, labels, col = col, ... )
}

# haloText <- function(x, y = NULL, labels, halo = FALSE, hw = 0.01, hc = "black",
#                      ...) {
#   xy <- xy.coords(x, y)
#
#   if (halo) {
#     xo <- hw * strwidth("A", cex = ifelse(c("cex") %in% names(list(...)),
#                                           list(...)[["cex"]],
#                                           par("cex")))
#     yo <- hw * strheight("A", cex = ifelse(c("cex") %in% names(list(...)),
#                                            list(...)[["cex"]],
#                                            par("cex")))
#     theta <- seq(pi / 8, 2 * pi, length.out = 16)
#     for (i in theta) {
#       text(xy[["x"]] + cos(i) * xo, xy[["y"]] + sin(i) * yo, labels,
#            col = hc, pos = 4, offset = 0,
#            cex = ifelse(c("cex") %in% names(list(...)),
#                         list(...)[["cex"]],
#                         par("cex")),
#            srt = ifelse(c("srt") %in% names(list(...)),
#                         list(...)[["srt"]],
#                         par("srt")))
#     }
#   }
#   text(xy[["x"]], xy[["y"]], labels,
#        pos = 4, offset = 0, ...)
# }
