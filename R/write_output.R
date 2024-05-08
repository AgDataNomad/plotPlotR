#' Write outputs
#'
#' Write outputs of **plotPlotR** such as experimental layouts as shapefile; supports most shapefile formats
#'
#' @param output_dat sf object to write to disk
#' @param file_name file name with file path and extension
#' @param overwrite OPTIONAL, TRUE to overwrite file
#'
#' @return
#' @export
#'
#' @examples
write_output_obj <- function(output_dat, file_name, overwrite = NULL){

  if (file.exists(file_name)){
    if (is.null(overwrite)) {
      stop("file already exist, use overwrite = TRUE to replace file")
    } else if (overwrite == TRUE){
        file.remove(file_name)
        st_write(obj = output_dat, dsn = file_name)
    } else if (overwrite  == FALSE){
        stop("File already exists and overwrite is set to FALSE")
      }
  } else {
    st_write(obj = output_dat, dsn = file_name)
  }
}
