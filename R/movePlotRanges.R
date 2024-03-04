#' Move ranges in your field experiment horizontally or vertically
#'
#' Run `find_offsets` before running `movePlotRanges`
#'
#' Move plots in your field experiment. This is unlikely as tractor sowing affects entire Run and almost never Ranges.
#'
#' @param sf_obj A sf object of geometry POLYGON or MULTIPOLYGON with Run and Range columns
#' @param moveDirection Direction of the move in reference Run 1 Range 1. Takes "up", "down", "left" or "right" as inputs.
#' @param moveDistance Numeric value in meters to move
#' @param rangeSelection Numeric value or numeric vector of a subset of ranges or all ranges
#'
#' @return a sf object of geometry POLYGON with Run and Range columns
#' @export
#'
#' @examples
#'
#' dat <- wgs84_to_unitM(cornersData, 28355)
#' dat_plots <- corners_to_plots(dat, 80, 24.2, 11, 20)
#' dat_plots_RR <- addRunRange(dat_plots, "BL")
#'
#' find_offsets(dat_plots_RR, "Run", "Range")
#'
#' moved_plots <- movePlotRanges(dat_plots_RR, "right", 0.1, c(1,3,5))
#'
#'
movePlotRanges <- function(sf_obj, moveDirection, moveDistance, rangeSelection){

  offset_values <- get("offset_values", offset_env)

  if (moveDirection %in% c("up", "down")){
    x_offset <- offset_values[3]
    y_offset <- offset_values[4]
  } else {
    x_offset <- offset_values[1]
    y_offset <- offset_values[1]
  }

  suppressWarnings(sfc_df <- as.data.frame.matrix(st_coordinates(st_centroid(sf_obj))))

  sfc_df$Run <- sf_obj$Run
  sfc_df$Range <- sf_obj$Range

  if (moveDirection %in% c("up", "right")) {
    add_fac <- 1
  } else {
    add_fac <- -1
  }

  sfc_df <- sfc_df %>%
    mutate(X = if_else(Range %in% rangeSelection,
                       (add_fac*moveDistance*x_offset), 0),
           Y = if_else(Range %in% rangeSelection,
                       (add_fac*moveDistance*y_offset), 0))

  sfc <- st_as_sf(sfc_df, coords = c("X", "Y"))

  sfc_g <- st_geometry(sfc)

  sf_g <- st_geometry(sf_obj)

  new_sf <- sf_g + sfc_g

  new_sf <- st_as_sf(new_sf)

  new_sf$Run <- sf_obj$Run
  new_sf$Range <- sf_obj$Range

  return(new_sf)

}
