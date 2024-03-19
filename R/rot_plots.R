#' Rotate individual plots on its center
#'
#' @param sf_object A MULTIPOLYGON sf object, representing plots on a field experiment.
#' @param rotation_angle A numerical value. Rotation angle in degrees. Positive or negative values accepted.
#' @return Returns a rotated sf object
#' @export
#'
#' @examples
rot_plots <- function(sf_object, rotation_angle){

  rotation_angle <- deg_2_rad(rotation_angle)

  a <- rot_shape(rotation_angle)

  centrd <- sf_object %>%
    st_geometry() %>%
    st_centroid()

  rot_plots <- (st_geometry(sf_object)-centrd)*a+centrd

  st_crs(rot_plots) <- st_crs(sf_object)

  return(rot_plots)

}
