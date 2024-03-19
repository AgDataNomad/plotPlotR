#' Convert degrees to radians
#'
#' @param degree_angle A numerical value. Angle in degrees.
#'
#' @return A numerical value degree in radians
#' @export
#'
#' @examples
deg_2_rad <- function(degree_angle){
  rad_angle <- degree_angle*(pi/180)
  return(rad_angle)
}
