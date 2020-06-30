#' Label the cross section with chemistry data
#'
#' This function labels each sampling location shown on the cross section with
#' analytical (or other) data.
#'
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
#' @param chem_id The names of the desired chemical labels. The names specified
#'   in chem_id should correspond with the column names of chem (see 'chem' below).
#' @param chem A data.frame of the chemical data to be plotted. The data.frame
#'   should contain columns corresponding to the chemical parameters that will
#'   be used to label the data.
#'
#' @return
#'
#' @keywords label, chemical
#'
#' @export
#'
#' @examples
#'
# label_chem("A", plot = TRUE, basemap = bm, col = "red")

label_chem <- function(distance, gs_elev, depth, tos, bos, chem_id, chem) {
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
