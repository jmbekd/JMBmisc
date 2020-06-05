library("data.table")
distance <- c(1, 1.1, 1.25)
gs_elev <- c( 2, 2.1, 2.5)
labels <- c("one", "two", "three")

loc_labels <- data.table(x = distance, y = gs_elev, label = labels)

plot(1:5, 1:5, type = "n")
label_data <- function(data,
                       n_iter = 5,
                       tolerance = 0.01,
                       shift_type = "min",
                       ...) {

  data <- unique(data)
  data <- data[!(is.na(label)), ] # Remove empty labels
  setorder(data, x, y) # Order data
  data[, id := .I] # adds row numbers
  data[, c("xo", "yo") := .(x, y)] # Save the original x, y coordinates
  data[, iter := 0]

  # Calculates bounding boxes for labels
  bb_data <<- copy(bb_labels(data, ...))
  cat("This is 'bb_data'\n")
  print(data[])
  cat("End of 'bb_data'\n\n")

  while (max(data[["iter"]]) <= n_iter) {
    # Calculate overlap
    overlap <- calc_overlap(data)
    overlap_data <<- copy(overlap)
    # dput(overlap)
    cat("This is the current 'overlap'\n")
    print(overlap[])
    cat("End of 'overlap'\n\n")

    if (nrow(overlap) == 0) {
      cat("No labels are overlapping\n\n")
      break
    }

    # Calculate shift
    shifts <- calc_shifts(overlap, tolerance, shift_type)
    shifts_data <<- copy(shifts)

    if (nrow(shifts) == 0) {
      cat("No significant overlaps\n\n")
      break
    }

    cat("This is the current 'shifts'\n")
    print(overlap[])
    cat("End of 'shifts'\n\n")

    # Shift labels
    shift_labels(data, shifts)
    shift_labels_data <<- copy(data)
    cat("This is the current step\n")
    print(i)
    cat("This is the current 'data'\n")
    print(data)
    cat("End of 'data'\n\n")
  }

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
label_data(data = loc_labels, cex = 1, srt = 90)

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
  # Multiplying the cex by 5/3 adds some vertical padding
  data[, height_str :=
         round(strheight(label,
                         cex = cex_val * 5 / 3), 6)]

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
  return(data[, c("width_str", "height_str") := NULL][])
}

calc_overlap <- function(data) {
  # From https://stackoverflow.com/questions/38445864/r-implementation-to-union-of-overlapping-rectangles
  overlap <- data[data,
                  .(id1 = pmin(i.id, x.id), id2 = pmax(i.id, x.id),
                    x0 = pmax(x.xleft, i.xleft), x1 = pmin(x.xright, i.xright),
                    y0 = pmax(x.ybottom, i.ybottom), y1 = pmin(x.ytop, i.ytop)),
                  on = .(xleft <= xright, xright >= xleft,
                         ybottom <= ytop, ytop >= ybottom),
                  by = .EACHI][id1 != id2]
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

  shifts <- copy(data)
  # Calculate the amount the label needs to shift
  shifts[, c("shift_x", "shift_y") :=
           .(round((x1 - x0) / 2, 6),
             round((y1 - y0) / 2, 6))]

  shift_x <- data[, round((x1 - x0) / 2, 6)]
  shift_y <- data[, round((y1 - y0) / 2, 6)]

  if (shift_type == "x") {
    cat("shift labels along x axis\n")
    shifts <- data[, .(id1, id2,
                       "shift_val" = shift_x,
                       "shift_axis" = "shift_x")]
  } else if (shift_type == "y") {
    cat("shift labels along y axis\n")
    shifts <- data[, .(id1, id2,
                       "shift_val" = shift_y,
                       "shift_axis" = "shift_y")]
  } else if (shift_type == "min") {
    shifts <- shifts[, shift_val :=
                       do.call(pmin, .SD),
                     .SDcols = c("shift_x", "shift_y")]
    shifts <- shifts[, "shift_axis" :=
                       colnames(.SD)[max.col(-.SD, ties.method = "first")],
                     .SDcols = c("shift_x", "shift_y")]
  }
  return(shifts[shift_val > tolerance,
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
         (shift_columns) := .SD + c(-1, 1) * shifts[["shift_val"]],
         .SDcols = shift_columns]
  }
  rows_shifted <- unique(unlist(shifts[, .(id1, id2)]))
  data[rows_shifted, iter := iter + 1]
  return(data)
}





old_shift_labels <- function(data, iteration) {
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
