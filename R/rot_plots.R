#' Rotate individual plots on its center
#'
#' @param sf_object A MULTIPOLYGON sf object, representing plots on a field experiment.
#' @param rotation_angle Rotation angle in radians. Use function rot_angle() to find rotation angle or pass radians as numeric input.
#'
#' @return Returns a rotated sf object
#' @export
#'
#' @examples
rot_plots <- function(sf_object, rotation_angle){

  rot_shape <- function(rot_angle){
    matrix(c(cos(rot_angle), sin(rot_angle), -sin(rot_angle), cos(rot_angle)), 2, 2)
  }

  a <- rot_shape(rotation_angle)

  centrd <- sf_object %>%
    st_geometry() %>%
    st_centroid()

  rot_plots <- (st_geometry(sf_object)-centrd)*a+centrd

  return(rot_plots)

}
