#' Orientation of field experiment layout
#'
#'This function takes a sf object of geometry MULTIPOLYGON or POINT and find it rotation on it center.
#'For example, a perfect North facing field experiment has a rotation of Zero degrees,
#'East would be 90 degrees and so on. Make sure data in Longitude or X and Latitude or Y order
#'
#' @param sf_object A sf object of geometry MULTIPOLYGON or POINT, or a data frame with 4 XY
#'
#' @return A numeric value of angle in radians
#' @export
#'
#' @examples
#'
#' rot_angle(cornersData)
#'
rot_angle <- function(sf_object){

  options(digits = 20)

  if("sf" %in% class(sf_object)){
    sf_object
  } else {
    sf_object <- sf_object %>%
      rename(X = 1, Y = 2) %>%
      st_as_sf(coords = c("X", "Y"))
  }

  suppressWarnings(corners_poly <- sf_object %>%
                     st_centroid() %>%
                     st_coordinates())


  corners_poly <- as.data.frame.matrix(corners_poly) %>%
    filter(X %in% c(min(X), max(X)) | Y %in% c(min(Y), max(Y)))

  x_dist <- max(corners_poly$X)-min(corners_poly$X)
  y_dist <- max(corners_poly$Y)-min(corners_poly$Y)

  if (x_dist<y_dist) {
    corners_poly <- corners_poly %>%
      arrange(Y, X)
  } else {
    corners_poly <- corners_poly %>%
      arrange(X, Y)
  }

  a <- corners_poly[,1]
  b <- corners_poly[,2]

  x <- (b[2]-b[1])/(a[2]-a[1])

  rotation_angle <- 360-((atan(x))*(180/pi))

  rotation_angle <- pi*(rotation_angle/180)

  rm(list = ls()[ls()!="rotation_angle"])

  if(rotation_angle>(360*(pi/180))){
    rotation_angle <- rotation_angle-(360*(pi/180))
  }else{
    rotation_angle
  }

  return(rotation_angle)

}
