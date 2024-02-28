#' Rotate an entire sf object
#'
#' @param rot_angle rotation angle in radians
#'
#' @return Rotated sf object
#' @export
#'
#' @examples
rot_shape <- function(rot_angle){

  matrix(c(cos(rot_angle), sin(rot_angle), -sin(rot_angle), cos(rot_angle)), 2, 2)
}
