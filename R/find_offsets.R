#' Find offsets to move plots horizontally and,or vertically
#'
#' Produces 1 plot unit offset values for moving the plots horizontally or vertically. If you
#'
#' @param sf_object A sf object of geometry POINT or POLYGON with Run and Range fields
#'
#' @return A matrix of X and Y offsets for horizontal and vertical shifts
#' @export
#'
#' @examples
#'
#' dat <- corners_to_plots(cornersData, 80, 24.2, 11, 20)
#'
#' dat_w_RR <- addRunRange(dat, "BL")
#'
#' find_offsets(dat_w_RR)
#'
find_offsets <- function(sf_object){

  options(digits = 20)

  suppressWarnings(sf_coords <- st_centroid(sf_object))

  sf_coords <- st_coordinates(sf_coords)

  sf_coords <- as.data.frame.matrix(sf_coords)

  sf_coords$Run <- sf_object$Run

  sf_coords$Range <- sf_object$Range

  run_offset_h <- sf_coords |>
    group_by(Range) %>%
    mutate(row_offset = lead(X)-X) %>%
    ungroup() %>%
    pull(row_offset) %>%
    mean(na.rm = TRUE)

  range_offset_h <- sf_coords |>
    group_by(Range) %>%
    mutate(range_offset = lead(Y)-Y) %>%
    ungroup() %>%
    pull(range_offset) %>%
    mean(na.rm = TRUE)

  run_offset_v <- sf_coords |>
    group_by(Run) %>%
    mutate(row_offset = lead(X)-X) %>%
    ungroup() %>%
    pull(row_offset) %>%
    mean(na.rm = TRUE)

  range_offset_v <- sf_coords |>
    group_by(Run) %>%
    mutate(range_offset = lead(Y)-Y) %>%
    ungroup() %>%
    pull(range_offset) %>%
    mean(na.rm = TRUE)

  a <- matrix(c(run_offset_h, range_offset_h,
                run_offset_v, range_offset_v), nrow = 2)

  return(a)

}
