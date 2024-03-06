#' Create split plot centers
#'
#' Splitting a single regular plot into two or three plots.
#'
#' @param sf_obj A sf object from a shape file, shp or geojson
#' @param split_dist Numeric input in meters indicating amount of offset to use in creating new plots.
#' @param split_into Integer input, number of new plots to generate from one plot, takes 1 or 2
#'
#' @return A sf object with geometry POINT
#' @export
#'
#' @examples
make_split_plots <- function(sf_obj, split_dist, split_into){

  if (!"offset_env" %in% ls(envir=.GlobalEnv)){
    stop("Run find_offsets first")
  }

  if (missing(split_into)){
    split_into <-  2
  } else {
    split_into
  }

  if (missing(split_dist)){
    split_dist <- 0.45
  } else {
    split_dist
  }

  offset_values <- get("offset_values", envir = offset_env)

  x_off <- offset_values[1]
  y_off <- offset_values[2]

  suppressWarnings(sf_cen <- sf_obj %>%
                     st_centroid() %>%
                     st_coordinates() %>%
                     as.data.frame())

  sf_off <- data.frame(X = rep((split_dist*x_off), nrow(sf_cen)),
                       Y = rep((split_dist*y_off), nrow(sf_cen)))
  left_sf <- sf_cen %>%
    mutate(X = X - sf_off$X,
           Y = Y - sf_off$Y) %>%
    st_as_sf(coords = c("X", "Y"))

  right_sf <- sf_cen %>%
    mutate(X = X + sf_off$X,
           Y = Y + sf_off$Y) %>%
    st_as_sf(coords = c("X", "Y"))

  cen_sf <- sf_cen %>%
    st_as_sf(coords = c("X", "Y"))

  if (split_into == 2) {
    split_sf <- bind_rows(left_sf, right_sf)
  } else {
    split_sf <- bind_rows(left_sf, cen_sf, right_sf)
  }

  st_crs(split_sf) <- st_crs(sf_obj)

  return(split_sf)

}
