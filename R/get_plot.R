#' Get raster for a specific plot or shape
#'
#' @param raster_obj Raster object as SpatRaster
#' @param vector_obj Shapefile as a SpatVector
#' @param plot_no OPTIONAL Plot number if it is a experimental layout. By default first shape is used.
#' @param RGB OPTIONAL, if TRUE, Raster will presented as a single layer RGB for better viz
#' @param stretchRGB OPTIONAL, if TRUE RGB values are stretched
#'
#' @return a raster object as SpatRaster
#' @export
#'
#' @examples
get_plot <- function(raster_obj, vector_obj, plot_no=NULL, RGB=NULL, stretchRGB=FALSE){

  if (is.null(plot_no)) {
    if (nrow(vector_obj)==1) {
      vector_obj
    }else if (nrow(vector_obj)>1) {
      stop("there are more than one plot and plot number is not supplied")
    }
  } else {
    vector_obj <- vector_obj[plot_no]
  }

  raster_obj <- crop(raster_obj,  vector_obj)
  raster_obj <- mask(raster_obj, vector_obj)

  if (is.null(RGB)){
    raster_obj
  } else if (RGB == TRUE){
    RGB(raster_obj) <- 1:3
  } else if (RGB == FALSE){
    raster_obj
  } else {
    stop("invalid input for argument RGB")
  }

  if (stretchRGB == TRUE) {
    raster_obj <- stretch(raster_obj)
  } else if (stretchRGB==FALSE) {
    raster_obj
  } else {
    stop("stretchRGB only takes logical values")
  }

  return(raster_obj)

}
