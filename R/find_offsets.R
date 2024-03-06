#' Find offsets to move plots horizontally and/or vertically
#'
#' Calculates 1 meter offset values for moving the plots horizontally or vertically to account for any discrepencies during sowing.
#'
#' @param sf_object A sf object of geometry POINT or POLYGON with Run and Range fields
#' @param runAlias Alias for Run in the sf object. Alias for Run could be row, run, Run, Row, ROW, RUN etc
#' @param rangeAlias Alias for Range in the sf object. Alias for Range could range, Range, col, Column column etc
#'
#' @return A matrix of X and Y offsets for horizontal and vertical moves
#' @export
#'
#' @examples
#'
#' dat <- wgs84_to_unitM(cornersData, 28355) #28355 is a unit M CRS
#'
#' dat_plots <- corners_to_plots(dat, 80, 24.2, 11, 20)
#'
#' dat_w_RR <- addRunRange(dat_plots, "BL")
#'
#' find_offsets(dat_w_RR, "Run", "Range")
#'
find_offsets <- function(sf_object, runAlias, rangeAlias){

  if (missing(runAlias)){
    runAlias <- "Run"
  } else {
    runAlias
  }

  if (missing(rangeAlias)){
    rangeAlias <- "Range"
  } else {
    rangeAlias
  }

  options(digits = 20)

  suppressWarnings(sf_coords <- st_centroid(sf_object))

  sf_coords <- st_coordinates(sf_coords)

  sf_coords <- as.data.frame.matrix(sf_coords)

  sf_coords$Run <- sf_object %>%
    st_drop_geometry() %>%
    select_at(runAlias) %>%
    pull() %>%
    as.numeric()

  sf_coords$Range <- sf_object %>%
    st_drop_geometry() %>%
    select_at(rangeAlias) %>%
    pull() %>%
    as.numeric()

  run_len <- sf_coords %>%
    group_by(Run) %>%
    filter(Range %in% c(min(Range), max(Range))) %>%
    arrange(Run, Range) %>%
    mutate(diff_y = diff(Y),
           diff_x = diff(X)) %>%
    mutate(run_dist = sqrt(diff_y^2+diff_x^2))

  ver_x <- mean(run_len$diff_x)/mean(run_len$run_dist)
  ver_y <- mean(run_len$diff_y)/mean(run_len$run_dist)

  range_len <- sf_coords %>%
    group_by(Range) %>%
    filter(Run %in% c(min(Run), max(Run))) %>%
    arrange(Range, Run) %>%
    mutate(diff_y = diff(Y),
           diff_x = diff(X)) %>%
    mutate(range_dist = sqrt(diff_y^2+diff_x^2))

  hor_x <- mean(range_len$diff_x)/mean(range_len$range_dist)
  hor_y <- mean(range_len$diff_y)/mean(range_len$range_dist)

  a <- matrix(c(hor_x, hor_y,
                ver_x, ver_y), ncol = 2)

  colnames(a) <- c("horizontal_move", "vertical_move")
  rownames(a) <- c("X_offset", "Y_offset")

  offset_env <<- new.env()

  assign("offset_values", a, offset_env)

  return(a)

}
