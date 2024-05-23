#' Create split plot centers
#'
#' Splitting a single regular plot into two or three plots.
#'
#' This is to support type of sowing where the seeder can sow more than one variety in each plot.
#'
#' @param plot_layout A sf object from a shape file, shp or geojson
#' @param split_dist Numeric input in meters indicating the amount of offset from plot center to use in creating new plots.
#' @param split_into Integer input, number of new plots to generate from one plot, takes 1 or 2
#' @param plot_length Numeric input, Length of plot in meters.
#' @param plot_width Numeric input, Width of plot in meters.
#'
#' @return A sf object with geometry POLYGON
#' @export
#'
#' @examples
#'
#' dat <- read_input_dat(cornersData, 4326, 28355)
#'
#' dat_plots <- corners_to_plots(dat, 80, 24.2, 11, 22)
#'
#' dat_split_plots <- make_split_plots(dat_plots, plot_length = 3, plot_width = 0.75)
#'
#' plot(dat_split_plots)
#'
make_split_plots <- function(plot_layout, split_dist=NULL, split_into=NULL, plot_length, plot_width){

  if (is.null(split_into)){
    split_into <-  2
  } else {
    split_into
  }

  if (is.null(split_dist)){
    split_dist <- 0.45
  } else {
    split_dist
  }

  x_off <- -(cos(rot_angle(plot_layout)))
  y_off <- (sin(rot_angle(plot_layout)))

  suppressWarnings(sf_cen <- plot_layout %>%
                     st_centroid() %>%
                     st_coordinates() %>%
                     as.data.frame())

  sfc_df <- bind_cols(sf_cen, st_drop_geometry(plot_layout))

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

  st_crs(split_sf) <- st_crs(plot_layout)

  split_sf <- make_plots(split_sf, plot_length = plot_length, plot_width = plot_width)

  return(split_sf)

}
