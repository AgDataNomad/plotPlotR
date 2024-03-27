#' Expand points
#'
#' For the purposes of creating flight plan when you know the corners of your experiment
#'
#' @param sf_obj A sf object of geometry type POINT
#' @param expand_by  A numeric value. Distance to expand the area in meters
#'
#' @return A sf object of geometry type POINT. CRS will be 4326
#' @export
#'
#' @examples
#'
#' dat <- read_input_dat(cornersData, 4326, 28355)
#'
#' dat_expanded <- expand_points(dat, 5)
#'
#' plot(dat)
#' plot(dat_expanded, add = TRUE)
#'
expand_points <- function(sf_obj, expand_by)  {

  options(digits = 20)

  makeClockwise <- function(sf_obj_df) {

      dat <- sf_obj_df %>%
        arrange(Y, X) %>%
        mutate(id = c("B","B","T","T")) %>%
        arrange(id, X) %>%
        mutate(id2 = c("L","R","L", "R")) %>%
        mutate(ord = paste0(id, id2)) %>%
        left_join(tibble(ord = c("BL", "TL", "TR", "BR"),
                         ord_by = 1:4), by = "ord") %>%
        arrange(ord_by) %>%
        select(X,Y)

      return(dat)
    }

  a <- st_crs(sf_obj)$input

  dat_df <- sf_obj %>%
    st_coordinates() %>%
    as.data.frame() %>%
    filter(X %in% c(min(X), max(X)) | Y %in% c(min(Y), max(Y)))

  dat_sf <- makeClockwise(dat_df)

  aX <- rep(c(-1,1), each = 2) * expand_by
  aY <- c(-1,1,1,-1) * expand_by

  dat_sf$X <- dat_sf$X+aX
  dat_sf$Y <- dat_sf$Y+aY

  dat_sf <- st_as_sf(dat_sf, coords = c("X", "Y"), crs = a)

  dat_sf <- st_transform(dat_sf, 4326)

  return(dat_sf)

}
