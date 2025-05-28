#' Get vegetation index from RGB raster
#'
#' @param raster_obj RGB raster
#' @param which_vi Select vegetation index. EXI, VDVI, NGDRI, MGRVI
#'
#' @return raster object of vegetation index
#' @export
#'
#' @examples
get_vi <- function(raster_obj, which_vi=NULL){

  if (is.null(which_vi)) {
    vi <- 2*(raster_obj[[2]])-(raster_obj[[1]])-(raster_obj[[3]])
  } else if(which_vi == "vi2") {
    vi <- 2*(raster_obj[[2]]/(raster_obj[[1]]+raster_obj[[2]]+raster_obj[[3]]))-(raster_obj[[1]]/(raster_obj[[1]]+raster_obj[[2]]+raster_obj[[3]]))-(raster_obj[[3]]/(raster_obj[[1]]+raster_obj[[2]]+raster_obj[[3]]))
  } else if (which_vi == "VDVI") {
    vi <- (2*raster_obj[[2]]-raster_obj[[1]]-raster_obj[[3]])/(2*raster_obj[[2]]+raster_obj[[1]]+raster_obj[[3]])
  } else if (which_vi == "NGDRI") {
    vi <- (raster_obj[[2]]-raster_obj[[1]])/(raster_obj[[2]]+raster_obj[[1]])
  } else if (which_vi == "VARI") {
    vi <- (raster_obj[[2]]-raster_obj[[1]])/(raster_obj[[2]]+raster_obj[[1]]-raster_obj[[3]])
  } else if (which_vi == "VEG") {
    vi <- raster_obj[[2]]/((raster_obj[[1]]^0.667)*(raster_obj[[3]]^(1-0.667)))
  } else if (which_vi == "MGRVI") {
    vi <- (raster_obj[[2]]^2-raster_obj[[1]]^2)/(raster_obj[[2]]^2+raster_obj[[1]]^2)
  }

  return(vi)
}
