#' Generating plots from four corner points
#'
#' This functions takes a sf object or similar with 4 corners of an field experiment,
#' find its center and rotation angle, make new plots based on other parameters
#' and produces a sf object with geometry type MULTIPOLYGON.
#'
#' Default plot dimensions are set to 90% of the max plot size without any plot gaps. Suits most use cases.
#'
#' For custom plot size, use optional plot size and plot width arguments
#'
#' Use `read_input_dat()` to bring in your corners data
#'
#'
#' @param sf_object A sf object with geometry of 4 POINTS or a data frame with four rows of X,Y (Longitude and Latitude)
#' @param exp_length Total length of the experiment in unit meter
#' @param exp_width Total width of the experiment in unit meter
#' @param n_runs Number of runs in the experiment, typically along the shorter side of the experiment.
#' @param n_ranges Number of ranges in the experiment, typically along the longer side of the experiment.
#' @param exp_orientation OPTIONAL. Orientation of runs in the experiment. Takes "NS" or "EW", Default is "NS". NS is North-South; EW is East-West
#' @param plot_length OPTIONAL. Numeric value. Length of a plot in meters
#' @param plot_width OPTIONAL. Numeric value. Width of a plot in meters
#'
#' @return A sf object of geometry MULTIPOLYGON with n_runs X n_ranges features.
#' @export
#'
#' @examples
#'
#'dat <- read_input_dat(cornersData, 4326)
#'
#'plots_dat <- corners_to_plots(dat, 80, 24.2, 11, 24)
#'
#'plot(plots_dat)
#'
corners_to_plots <- function(sf_object, exp_length, exp_width, n_runs, n_ranges,
                             exp_orientation = NULL,
                             plot_length = NULL, plot_width = NULL){

  if (is.null(exp_orientation)){
    if (1.5*exp_length>exp_width){
      rotation_angle <- rot_angle(sf_object=sf_object)
      print("make sure the orientation is correct!!")
    } else {
      rotation_angle <- rot_angle(sf_object=sf_object)+deg_2_rad(90)
      print("make sure the orientation is correct!!")
    }
  } else if (exp_orientation=="NS") {
    rotation_angle <- rot_angle(sf_object=sf_object)
  } else if (exp_orientation == "EW"){
    rotation_angle <- rot_angle(sf_object=sf_object)+deg_2_rad(90)
  } else {
    stop("exp_orientation expects a value of 'NS' or 'EW'")
  }

  if("sf" %in% class(sf_object)){
    sf_object
  } else {
    sf_object <- sf_object %>%
      rename(X = 1, Y = 2) %>%
      st_as_sf(coords = c("X", "Y"))
  }

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

  if (!(is.null(plot_length) & is.null(plot_width))){
    plot_centers <- make_plots(plot_centers, plot_length = plot_length, plot_width = plot_width)
  } else {
    plot_centers
  }

  return(plot_centers)

}
