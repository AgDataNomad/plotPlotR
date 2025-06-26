#' Extract individual plots photos from raster
#'
#' @param raster A raster object
#' @param plot_layout A sf object containing polygons
#' @param plot_subset Subset of plots to be extracted rather than all plots
#' @param dir_loc A directory location to write files
#' @param file_type ".png" or ".tif" type images to write
#'
#' @return progress message
#' @export
#'
#' @examples
extract_plots_from_rasters <- function(raster,
                                       plot_layout,
                                       plot_subset = NULL,
                                       dir_loc=NULL,
                                       file_type=".png"){

  if(!file_type%in%c(".png", ".tif")) stop("file type only accepts '.png' or 'tif as values")

  if(is.null(dir_loc)){
    warning("Files will be created in the current working directory")
  }else{
    if(!dir.exists(dir_loc)){
      warning(paste("dir doesn't exist and will attempt to create directory"))
      dir.create(dir_loc)
      if(!dir.exists(dir_loc)){
        stop("directory creation failed! Check value supplied")
      }
    }
  }

  if(is.null(plot_subset)){
    leading_zeros <- nchar(nrow(plot_layout))
  }else{
    if(plot_subset>=nchar(nrow(plot_layout))){
      leading_zeros <- nchar(nrow(plot_layout))
    } else{
      leading_zeros <- plot_subset
    }
  }


  leading_zeros <- paste0("%0",leading_zeros,"d")

  for (i in (1:nrow(plot_layout))){

    print(paste("Writing plot", i))

    plot_sub <- plot_layout[i,]

    image_i <- crop(rgb, plot_sub)
    image_i <- mask(image_i, plot_sub)

    writeRaster(image_i,
                paste0(out_path,
                       "plot_",
                       sprintf(leading_zeros,i),
                       file_type))

  }

}
