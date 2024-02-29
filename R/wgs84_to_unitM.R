#' Converting coordinate to a unit M CRS
#'
#' @param sf_object A sf object or a data frame that with X and Y columns containing Longitude and Latitude data
#' @param CRSinMeters CRS that is in unit M. Check epsg.io to find appropriate CRS for your location
#'
#' @return A sf with unit M CRS
#' @export
#'
#' @examples
#'
#' wgs84_to_unitM(cornersData, 28355)
#'
wgs84_to_unitM <- function(sf_object, CRSinMeters){

  if("sf" %in% class(sf_object)){
    sf_object <- sf_object %>%
      st_set_crs(4326) %>%
      st_transform(CRSinMeters)
  } else {
    sf_object <- sf_object %>%
      rename(X = 1, Y = 2) %>%
      st_as_sf(coords = c("X", "Y"))%>%
      st_set_crs(4326) %>%
      st_transform(CRSinMeters)
  }

  return(sf_object)

}
