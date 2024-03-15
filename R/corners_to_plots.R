#' Generating plots from four corner points
#'
#' This functions takes a sf object or similar with 4 corners of an field experiment,
#' find its center and rotation angle, make new plots based on other parameters
#' and produces a sf object with geometry type MULTIPOLYGON.
#'
#' Plot dimensions are set to 90% of the max plot size without any plot gaps. Suits most use cases.
#'
#' For custom plot size, use `make_plots()` function to adjust plot size and plot width
#'
#' Use `read_input_dat()` to bring you corners data
#'
#'
#' @param sf_object A sf object with geometry of 4 POINTS or a data frame with four rows of X,Y (Longitude and Latitude)
#' @param exp_length Total length of the experiment in unit meter
#' @param exp_width Total width of the experiment in unit meter
#' @param n_runs Number of runs in the experiment, typically along the shorter side of the experiment.
#' @param n_ranges Number of ranges in the experiment, typically along the longer side of the experiment.
#'
#' @return A sf object of geometry MULTIPOLYGON with n_runs X n_ranges features.
#' @export
#'
#' @examples
#'
#'dat <- read_input_dat(cornersData)
#'
#'plots_dat <- corners_to_plots(dat, 80, 24.2, 11, 24)
#'
#'plot(plots_dat)
#'
corners_to_plots <- function(sf_object, exp_length, exp_width, n_runs, n_ranges){

  a_ls <- ls()
  a_c <- names(as.list(match.call()))[-1]
  if (any(!a_ls %in% a_c)) {
    stop(paste("missing values for", paste(setdiff(a_ls, a_c), collapse=", ")))
  }

  if("sf" %in% class(sf_object)){
    sf_object
  } else {
    sf_object <- sf_object %>%
      rename(X = 1, Y = 2) %>%
      st_as_sf(coords = c("X", "Y"))
  }

  rotation_angle <- rot_angle(sf_object=sf_object)

  centrd <- sf_object %>%
    st_union() %>%
    st_centroid()

  inc_by <- (exp_length-exp_width)/2

  big_poly <- st_buffer(centrd,
                        dist = exp_width/2,
                        endCapStyle = "SQUARE")

  big_poly <- st_cast(big_poly, "POLYGON")

  bp_coords <- as.data.frame.matrix(st_coordinates(big_poly))

  bp_coords <- bp_coords %>%
    arrange(Y,X)

  bp_coords$Y <- bp_coords$Y + (inc_by*c(-1,-1,1,1,1))

  bp_coords_ <- bp_coords %>%
    st_as_sf(coords = c("X", "Y")) %>%
    mutate(ord = c(1,5,2,4,3)) %>%
    arrange(ord) %>%
    dplyr::summarise(do_union = FALSE) %>%
    st_cast("LINESTRING") %>%
    st_cast("POLYGON")

  bp_grid <- st_make_grid(bp_coords_, n=c(n_runs, n_ranges))

  bp_centers <- st_centroid(bp_grid)

  centrd <- st_centroid(st_union(bp_centers))

  plot_centers <- (bp_grid - centrd) * rot_shape(rotation_angle) + centrd

  plot_centers <- (st_geometry(plot_centers)-st_centroid(plot_centers))*0.9+st_centroid(plot_centers)

  plot_centers <- st_as_sf(plot_centers)

  st_geometry(plot_centers) <- "geometry"

  if (is.na(st_crs(sf_object)$input)==TRUE){
    plot_centers
  } else {
    st_crs(plot_centers) <- st_crs(sf_object)
  }

  return(plot_centers)

}
