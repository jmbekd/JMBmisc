library("data.table")
distance <- c(1, 1.1, 1.2)
gs_elev <- c(1, 1.2, 1.3)
labels <- c("oneM", "twoj", "threeg")

loc_labels <- data.table(x = distance, y = gs_elev, label = labels)

plot(1:5, 1:5, type = "n")
loc_labels[, points(x, y, pch = 19)]
label_data <- function(data,
                       shift = TRUE,
                       n_iter = 5,
                       tolerance = 0.01,
                       shift_type = "min",
                       halo = FALSE,
                       col = "black",
                       hc = "white",
                       hw = 0.01,
                       ...) {

  if (shift == TRUE) {
    data <- unique(data)
    data <- data[!(is.na(label)), ] # Remove empty labels
    setorder(data, x, y) # Order data
    data[, id := .I] # adds row numbers
    data[, c("xo", "yo") := .(x, y)] # Save the original x, y coordinates
    data[, iter := 0]

    # Calculates bounding boxes for labels
    bb_labels(data, ...)

    while (max(data[["iter"]]) <= n_iter) {
      # Calculate overlap
      overlap <- calc_overlap(data)
      if (nrow(overlap) == 0) {
        cat(paste0("After ", max(data[["iter"]]),
                   " iteration(s) there are no overlapping labels.\n\n"))
        break
      }

      # Calculate shift
      shifts <- calc_shifts(overlap, tolerance, shift_type)
      if (nrow(shifts) == 0) {
        cat(paste0("After ", max(data[["iter"]]),
                   " iteration(s) there are no significant overlaps.\n\n"))
        break
      }

      # Shift labels
      shift_labels(data, shifts)
    }
  }

  #### Plots labels ----
  # For the labeling to work, require 'pos = 4' and 'offset = 0'
  # If 'srt = 0', the 'x' coordinate is left most coordinate of the
  # text bounding box (i.e., x0) and the 'y' coordinate is the vertical
  # midpoint text bounding box (i.e., (y0 + y1) / 2).
  # If 'srt = 90', the 'x' coordinate is the horizontal midpoint of the
  # text bounding box (i.e., (x0 + x1) / 2) and the 'y' coordinate is
  # the bottom most coordinate of the bounding box (i.e., y0)
  data[, halotext(x, y, label, col,
                  halo, hw, hc,
                  pos = 4, offset = 0,
                  ...)]
  #### Plots rectangles around text - Useful for error checking ----
  # data[, rect(xleft,
  #             ybottom,
  #             xright,
  #             ytop,
  #             border = .I), by = .I]
return(data[])
}
label_data(data = loc_labels, cex = 1, srt = 0)

bb_labels <- function(data, ...) { # Calculates bounding boxes for text labels
  cex_val <- ifelse(c("cex") %in% names(list(...)),
                    list(...)[["cex"]],
                    par("cex"))
  srt_val <- ifelse(c("srt") %in% names(list(...)),
                    list(...)[["srt"]],
                    par("srt"))

  ## Calculates length and width of labels
  # Adds some horizontal padding for bounding box
  data[, label := paste0(" ", label, " ")]
  data[, width_str :=
         round(strwidth(label,
                        cex = cex_val), 6)]
  # Multiplying the cex by 4/3 adds some vertical padding
  data[, height_str :=
         round(strheight(label,
                         cex = cex_val * 4 / 3), 6)]

  # Calculates bounding boxes for text
  plot_usr <- par("usr")
  data[, xleft := x]
  data[, xright := x + width_str]
  data[, ybottom := y - height_str / 2]
  data[, ytop := y + height_str / 2]

  if (srt_val == 90) {
    data[, temp := width_str]
    data[, width_str := height_str /
           (plot_usr[4] - plot_usr[3]) * par("pin")[2]]
    data[, width_str := width_str /
           par("pin")[1] * (plot_usr[2] - plot_usr[1])]
    data[, height_str := temp /
           (plot_usr[2] - plot_usr[1]) * par("pin")[1]]
    data[, height_str := height_str /
           par("pin")[2] * (plot_usr[4] - plot_usr[3])]
    data[, temp := NULL] # removes temporary column
    data[, width_str := round(width_str, 6)] # forces rounding to 6 sig figs
    data[, height_str := round(height_str, 6)] # forces rounding to 6 sig figs

    data[, xleft := x - width_str / 2]
    data[, xright := x + width_str / 2]
    data[, ybottom := y]
    data[, ytop := y + height_str]
  }
  data[, c("width_str", "height_str") := NULL]
  return(data)
}

calc_overlap <- function(data) {
  # From https://stackoverflow.com/questions/38445864/r-implementation-to-union-of-overlapping-rectangles
  overlap <- data[data,
                  .(id1 = pmin(i.id, x.id), id2 = pmax(i.id, x.id),
                    x0 = pmax(x.xleft, i.xleft), x1 = pmin(x.xright, i.xright),
                    y0 = pmax(x.ybottom, i.ybottom), y1 = pmin(x.ytop, i.ytop)),
                  on = .(xleft <= xright, xright >= xleft,
                         ybottom <= ytop, ytop >= ybottom),
                  allow.cartesian = TRUE][id1 != id2]
  overlap <- unique(overlap)
  overlap <- overlap[, .SD[1],
                     .SDcols = c("id1", "id2", "x0", "x1", "y0", "y1"),
                     by = .("rects" = paste(id1, id2, sep = "_"))]
  return(overlap[])
}

calc_shifts <- function(data, tolerance = 0.01, shift_type = "min") {
  if (!(shift_type %in% c("x", "y", "min"))) {
    stop(cat("shift_type must be one of 'x', 'y', or 'min'.\n"))
  }

  # Calculate the amount the label needs to shift
  data[, c("shift_x", "shift_y") :=
         .(round((x1 - x0) / 2, 6),
           round((y1 - y0) / 2, 6))]

  if (shift_type == "x") {
    data[, c("shift_val", "shift_axis") :=
           .(shift_x, "shift_x")]
  } else if (shift_type == "y") {
    data[, c("shift_val", "shift_axis") :=
           .(shift_y, "shift_y")]
  } else if (shift_type == "min") {
    data[, shift_val :=
           do.call(pmin, .SD),
         .SDcols = c("shift_x", "shift_y")]
    data[, shift_axis :=
           colnames(.SD)[max.col(-.SD, ties.method = "first")],
         .SDcols = c("shift_x", "shift_y")]
  }
  return(data[shift_val > tolerance,
                .(id1, id2, shift_val, shift_axis)])
}

shift_labels <- function(data, shifts) {
  for (i in 1:nrow(shifts)) {
    if (shifts[i, shift_axis] == "shift_x") {
      shift_columns <- c("x", "xleft", "xright")
    } else {
      shift_columns <- c("y", "ybottom", "ytop")
    }
    rows_to_shift <- unlist(shifts[i, .(id1, id2)])
    data[rows_to_shift,
         (shift_columns) := .SD + c(-1, 1) * shifts[["shift_val"]][i],
         .SDcols = shift_columns]
  }
  rows_shifted <- unique(unlist(shifts[, .(id1, id2)]))
  data[rows_shifted, iter := iter + 1]
  return(data)
}

# add.alpha function from https://gist.github.com/mages/5339689
add.alpha <- function(cols, alpha) rgb(t(col2rgb(cols) / 255),
                                       alpha = alpha)

#from TeachingDemos package
halotext <- function(x, y = NULL, labels, col = "black",
                     halo = FALSE, hw = 0.01, hc = "white",
                     theta = seq(pi / 8, 2 * pi, length.out = 16),
                     ...) {

  cex_val <- ifelse(c("cex") %in% names(list(...)),
                    list(...)[["cex"]],
                    par("cex"))

  xy <- xy.coords(x, y)
  if (halo == TRUE) {
    xo <- hw * strwidth("A", cex = cex_val)
    yo <- hw * strheight("A", cex = cex_val)

    for (i in theta) {
      text(xy$x + cos(i) * xo,
           xy$y + sin(i) * yo,
           labels,
           col = hc, ...)
    }
  }
  text(xy$x, xy$y, labels, col = col, ...)
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
