#' Write outputs
#'
#' Write outputs as shapefile
#'
#' suppors most of the shapefile formats
#'
#' @param output_dat sf object to write to disk
#' @param file_name file name with file path and extension
#'
#' @return
#' @export
#'
#' @examples
write_output_obj <- function(output_dat, file_name){
  st_write(obj = output_dat, dsn = file_name, delete_dsn=TRUE)
}
