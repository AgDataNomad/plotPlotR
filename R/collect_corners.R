#' Collect corners of a raster
#'
#' This will temporarily load raster in your environment
#' Expect this process to be slow
#'
#'
#' @param raster_object A raster object or path to raster object
#' @param numPoints Integer value. Has to be multiples of 4. Each 4 points should have the same center as the experiment.
#'
#' @return A sf object with corners of experiment from a raster
#' @export
#'
#' @examples
collect_corners <- function(raster_object, numPoints=NULL) {

  if(is.null(numPoints)){
    numPoints <- 4
  } else if (numpoints%%4 != 0) {
    stop("numPoints has to be multiple of 4")
  } else {
    numPoints
  }

  if (grepl("Raster", class(raster_object))) {
    dat <- raster_object
  } else if (inherits(raster_object, "character")){
    if(file.exists(raster_object)){
      dat <- rast(raster_object, lyrs = 1)
    } else {
        stop("File doesn't exist, check your file path")
      }
  } else {
    stop("Not a vaid input!")
  }

  dev.new()

  terra::plot(dat)

  dat <- click(dat, n=numPoints, xy=TRUE, show = FALSE)

  dat <- dat %>%
    select(1:2) %>%
    rename(X=1, Y=2) %>%
    st_as_sf(coords = c("X","Y"))

  st_crs(dat) <- st_crs(raster_object)

  return(dat)

}
