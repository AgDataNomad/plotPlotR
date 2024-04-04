#' Move ranges in your field experiment horizontally or vertically
#'
#' Users can select a subset of ranges or all ranges
#'
#' Move distance is in unit meters
#'
#' Move direction is in relation to the position of Run 1 and Range 1
#'
#' Move plots in your field experiment.
#'
#' This is unlikely as tractor sowing affects entire Run and almost never a subset of Ranges.
#'
#' @param sf_obj A sf object of geometry POLYGON or MULTIPOLYGON with Run and Range columns
#' @param moveDirection Direction of the move in reference Run 1 Range 1. Takes "up", "down", "left" or "right" as inputs.
#' @param moveDistance Numeric value in meters to move
#' @param rangeSelection Integer value or vector of a subset of ranges or all ranges. Default is all ranges.
#'
#' @return a sf object of geometry POLYGON with Run and Range columns
#' @export
#'
#' @examples
#'
#' dat <- read_input_dat(cornersData, 4326, 28355)
#' dat_plots <- corners_to_plots(dat, 80, 24.2, 11, 20)
#' dat_plots_RR <- addRunRange(dat_plots, "BL", 11, 20)
#'
#'
#' moved_plots <- movePlotRanges(dat_plots_RR, "right", 1.2, c(1,3,9))
#'
#' plot(moved_plots)
#'
#'
movePlotRanges <- function(sf_obj, moveDirection, moveDistance, rangeSelection=NULL){

  if (is.null(rangeSelection)){
    rangeSelection <- 1:max(sf_obj$Range)
  } else {
    rangeSelection
  }

  if (moveDirection %in% c("left", "right")){
    x_offset <- abs(cos(rot_angle(sf_obj)))
    y_offset <- abs(sin(rot_angle(sf_obj)))
  } else {
    x_offset <- abs(sin(rot_angle(sf_obj)-pi))
    y_offset <- abs(cos(rot_angle(sf_obj)-pi))
  }

  suppressWarnings(sfc_df <- as.data.frame(st_coordinates(st_centroid(sf_obj))))

  sfc_df <- bind_cols(sfc_df, st_drop_geometry(sf_obj))

  ardf <- sfc_df %>%
    filter(X %in% c(min(X), max(X)) | Y %in% c(min(Y), max(Y)))

  mvRunX <- ardf %>%
    filter(Run == 1) %>%
    arrange(Run, Range) %>%
    pull(X) %>%
    diff() %>%
    sign()

  mvRunY <- ardf %>%
    filter(Run == 1) %>%
    arrange(Run, Range) %>%
    pull(Y) %>%
    diff() %>%
    sign()

  mvRangeX <- ardf %>%
    filter(Range == 1) %>%
    arrange(Run, Range) %>%
    pull(X) %>%
    diff() %>%
    sign()

  mvRangeY <- ardf %>%
    filter(Range == 1) %>%
    arrange(Run, Range) %>%
    pull(Y) %>%
    diff() %>%
    sign()

  if (moveDirection == "left"){
    addFacX <- -mvRangeX
    addFacY <- -mvRangeY
  } else if (moveDirection == "right"){
    addFacX <- mvRangeX
    addFacY <- mvRangeY
  } else if (moveDirection == "up"){
    addFacX <- mvRunX
    addFacY <- mvRunY
  } else if (moveDirection == "down"){
    addFacX <- -mvRunX
    addFacY <- -mvRunY
  }

  sfc_df <- sfc_df %>%
    mutate(X = if_else(Range %in% rangeSelection,
                       (addFacX*moveDistance*x_offset), 0),
           Y = if_else(Range %in% rangeSelection,
                       (addFacY*moveDistance*y_offset), 0))

  sfc <- st_as_sf(sfc_df, coords = c("X", "Y"))

  new_sf <- st_geometry(sf_obj) + st_geometry(sfc)

  new_sf <- st_as_sf(new_sf)

  new_sf <- bind_cols(new_sf, st_drop_geometry(sf_obj))

  st_geometry(new_sf) <- "geometry"

  st_crs(new_sf) <- st_crs(sf_obj)

  return(new_sf)
}
