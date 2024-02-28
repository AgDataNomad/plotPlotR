#' Multi-corner points to 4 corner points
#'
#' To increase the accuracy of finding the center point of an field experiment,
#' users can use this function where multiple points for each corner is collected.
#' This function then use all those points to find the average corner points and
#' improve the precision of the center.
#'
#' @param sf_object A sf object or a data frame with three columns, geometry and
#' an id column to group by corners. The data frame needs to contain columns Longitude,
#' Latitude and id.
#'
#' @return sf object with 4 points
#' @export
#'
#' @examples
multicorners_to_4corners <- function(sf_object){

  if("sf" %in% class(sf_object)){
    sf_object
  } else {
    sf_object <- sf_object %>%
      rename(X = 1, Y = 2, id = 3) %>%
      st_as_sf(coords = c("X", "Y"))
  }

  sf_object <- sf_object %>%
    group_by(id) %>%
    st_union() %>%
    st_centroid() %>%
    ungroup()

  return(sf_object)
}
