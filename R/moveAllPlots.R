#' Move all plots
#'
#' Move all the plots in your experiment to a new center.
#'
#' @param sf_obj sf object to type POLYGON to move.
#' @param xy_move a numeric vector of length 2. X and Y move distance in meters.
#'
#' @return A sf object
#' @export
#'
#' @examples
moveAllPlots <- function(sf_obj, xy_move){

  if (length(xy_move)!=2){
    stop("Provide a vector of length 2")
  } else if (!inherits(xy_move, "numeric")){
    stop("Please provide numeric values")
  } else {
    xy_move
  }

  feature_length <- length(st_geometry(sf_obj))

  adjusted_sf <- data.frame(X = rep(xy_move[1], feature_length),
                            Y = rep(xy_move[2], feature_length))

  adjusted_sf <- st_as_sf(adjusted_sf, coords = c("X","Y"))

  adjusted_sf <- st_geometry(adjusted_sf)

  new_sf <- st_geometry(sf_obj)+adjusted_sf

  new_sf <- st_as_sf(new_sf)

  new_sf <- bind_cols(new_sf, st_drop_geometry(sf_obj))

  st_geometry(new_sf) <- "geometry"

  st_crs(new_sf) <- st_crs(sf_obj)

  return(new_sf)

}
