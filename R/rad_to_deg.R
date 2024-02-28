#' radian to degree
#'
#' Convert radians to degree angle.
#'
#'
#' @param rad_value Rotation angle in radian.
#'
#' @return A numeric value of degree angle. Values will be between Zero and 360.
#' @export
#'
#' @examples
rad_2_deg <- function(rad_value){

  deg_value <- rad_value*(180/pi)

  if (deg_value > 360){
    deg_value <- deg_value-360
  } else {
    deg_value
  }
  return(deg_value)
}
