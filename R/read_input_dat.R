#' Read input data
#'
#' @param input_dat CSV file containing XY or any shape file formats.
#' @param input_CRS Integer input. CRS of input data as EPSG code
#' @param to_CRS_M Integer input. Corresponding CRS in unit M to input CRS as EPSG code
#' @param XY a vector identifying colnames of X and Y e.g. c("Easting", "Northing")
#'
#' @return A sf object with CRS
#' @export
#'
#' @examples
#' cornersData
#'
#' dat <- read_input_dat(cornersData, 4326, 28355)
#'
#' dat
#'
read_input_dat <- function(input_dat, input_CRS, to_CRS_M, XY = NULL){

  if (!is.null(XY)){
    if ("data.frame" %in% class(input_dat)){
      dat <- input_dat %>%
        rename(X=XY[1], Y=XY[2]) %>%
        st_as_sf(coords = c("X","Y"))
    } else if (tolower(substr(input_dat, nchar(input_dat)-2,nchar(input_dat))) == "csv") {
      dat <- read.csv(input_dat)
      dat <- dat %>%
        rename(X=XY[1], Y=XY[2]) %>%
        st_as_sf(coords = c("X", "Y"))
    } else {
      dat <- st_read(input_dat)
    }
  } else {
    if ("data.frame" %in% class(input_dat)){
      dat <- input_dat %>%
        rename(X=1, Y=2) %>%
        st_as_sf(coords = c("X","Y"))
    } else if (tolower(substr(input_dat, nchar(input_dat)-2,nchar(input_dat))) == "csv") {
      dat <- read.csv(input_dat)
      colnames(dat)[1] <- "X"
      colnames(dat)[2] <- "Y"
      dat <- st_as_sf(dat, coords = c("X", "Y"))
    } else {
      dat <- st_read(input_dat)
    }
  }

  if (!is.na(st_crs(dat)$input)){
    if (st_is_longlat(dat) & missing(to_CRS_M)){
      dat <- st_transform(dat, 3857)
    } else {
      dat
    }
  } else if (missing(input_CRS)) {
    stop("Please provide value for input CRS")
  } else if (missing(to_CRS_M)) {
    st_crs(dat) <- input_CRS
    if (st_is_longlat(dat)) {
      dat <- st_transform(dat, 3857)
    } else {
      st_crs(dat) <- input_CRS
    }
  } else {
    st_crs(dat) <- input_CRS
    dat <- st_transform(dat, to_CRS_M)
  }

  return(dat)

}
