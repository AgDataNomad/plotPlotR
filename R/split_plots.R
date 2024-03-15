#' Create split plot centers
#'
#' Splitting a single regular plot into two or three plots.
#'
#' This is to support type of sowing where the seeder can sow more than one variety in each plot.
#'
#' @param sf_obj A sf object from a shape file, shp or geojson
#' @param split_dist Numeric input in meters indicating the amount of offset from plot center to use in creating new plots.
#' @param split_into Integer input, number of new plots to generate from one plot, takes 1 or 2
#'
#' @return A sf object with geometry POINT
#' @export
#'
#' @examples
#'
#' dat <- read_input_dat(cornersData, 4326, 28355)
#'
#' dat_plots <- corners_to_plots(dat, 80, 24.2, 11, 22)
#'
#' dat_split_plots <- make_split_plots(dat_plots)
#'
#' plot(dat_split_plots)
#'
make_split_plots <- function(sf_obj, split_dist, split_into){

  if (missing(split_into)){
    split_into <-  2
  } else {
    split_into
  }

  if (missing(split_dist)){
    split_dist <- 0.45
  } else {
    split_dist
  }

  x_off <- -(cos(rot_angle(sf_obj)))
  y_off <- (sin(rot_angle(sf_obj)))

  suppressWarnings(sf_cen <- sf_obj %>%
                     st_centroid() %>%
                     st_coordinates() %>%
                     as.data.frame())

  sfc_df <- bind_cols(sf_cen, st_drop_geometry(sf_obj))

  ardf <- sfc_df %>%
    filter(X %in% c(min(X), max(X)) | Y %in% c(min(Y), max(Y)))

  sf_off <- data.frame(X = rep((split_dist*x_off), nrow(sf_cen)),
                       Y = rep((split_dist*y_off), nrow(sf_cen)))
  left_sf <- sf_cen %>%
    mutate(X = X - sf_off$X,
           Y = Y - sf_off$Y) %>%
    st_as_sf(coords = c("X", "Y"))

  right_sf <- sf_cen %>%
    mutate(X = X + sf_off$X,
           Y = Y + sf_off$Y) %>%
    st_as_sf(coords = c("X", "Y"))

  cen_sf <- sf_cen %>%
    st_as_sf(coords = c("X", "Y"))

  if (split_into == 2) {
    split_sf <- bind_rows(left_sf, right_sf)
  } else {
    split_sf <- bind_rows(left_sf, cen_sf, right_sf)
  }

  st_crs(split_sf) <- st_crs(sf_obj)

  return(split_sf)

}
