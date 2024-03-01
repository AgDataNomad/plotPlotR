#' Find offsets to move plots horizontally and/or vertically
#'
#' Produces 1 plot unit offset values for moving the plots horizontally or vertically. If you
#'
#' @param sf_object A sf object of geometry POINT or POLYGON with Run and Range fields
#' @param runAlias Alias for Run in the sf object. Alias for Run could be row, run, Run, Row, ROW, RUN etc
#' @param rangeAlias Alias for Range in the sf object. Alias for Range could range, Range, col, Column column etc
#'
#' @return A matrix of X and Y offsets for horizontal and vertical shifts
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

  run_offset_h <- sf_coords %>%
    arrange(Run) %>%
    group_by(Range) %>%
    mutate(row_offset = lead(X)-X) %>%
    ungroup() %>%
    pull(row_offset) %>%
    mean(na.rm = TRUE)

  range_offset_h <- sf_coords |>
    arrange(Run) %>%
    group_by(Range) %>%
    mutate(range_offset = lead(Y)-Y) %>%
    ungroup() %>%
    pull(range_offset) %>%
    mean(na.rm = TRUE)

  run_offset_v <- sf_coords |>
    arrange(Range) %>%
    group_by(Run) %>%
    mutate(row_offset = lead(X)-X) %>%
    ungroup() %>%
    pull(row_offset) %>%
    mean(na.rm = TRUE)

  range_offset_v <- sf_coords |>
    arrange(Range) %>%
    group_by(Run) %>%
    mutate(range_offset = lead(Y)-Y) %>%
    ungroup() %>%
    pull(range_offset) %>%
    mean(na.rm = TRUE)

  a <- matrix(c(run_offset_h, range_offset_h,
                run_offset_v, range_offset_v), nrow = 2)

  return(a)

}
