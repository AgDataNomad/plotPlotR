#' Rotate entire experiment or individual plots on its center
#'
#' @param sf_object A MULTIPOLYGON sf object, representing plot layouts on a field experiment.
#' @param rotation_angle A numerical value. Rotation angle in degrees. Positive or negative values accepted.
#' @param selection A character string of value "plots" or "experiment". "Plots" rotates individual plots on its center, "experiment" rotates the entire experiment.
#' @return Returns a rotated sf object
#' @export
#'
#' @examples
rot_plots <- function(sf_object, rotation_angle, selection){

  rotation_angle <- deg_2_rad(rotation_angle)

  a <- rot_shape(rotation_angle)

  if (selection == "plots") {

    centrd <- sf_object %>%
      st_geometry() %>%
      st_centroid()

    rot_obj <- (st_geometry(sf_object)-centrd)*a+centrd
  } else if (selection == "experiment") {

    centrd <- st_centroid(st_union(st_centroid(st_geometry(sf_object))))

    rot_obj <- (st_geometry(sf_object)-centrd)*a+centrd
  }

  st_crs(rot_obj) <- st_crs(sf_object)

  rot_obj <- st_as_sf(rot_obj)

  rot_obj <- bind_cols(rot_obj, st_drop_geometry(sf_object))

  st_geometry(rot_obj) <- "geometry"

  return(rot_obj)

}
