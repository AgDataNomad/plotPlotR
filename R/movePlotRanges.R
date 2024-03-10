#' Move ranges in your field experiment horizontally or vertically
#'
#' Run `find_offsets` before running `movePlotRanges`
#'
#' Move plots in your field experiment. This is unlikely as tractor sowing affects entire Run and almost never Ranges.
#'
#' @param sf_obj A sf object of geometry POLYGON or MULTIPOLYGON with Run and Range columns
#' @param moveDirection Direction of the move in reference Run 1 Range 1. Takes "up", "down", "left" or "right" as inputs.
#' @param moveDistance Numeric value in meters to move
#' @param rangeSelection Numeric value or numeric vector of a subset of ranges or all ranges
#'
#' @return a sf object of geometry POLYGON with Run and Range columns
#' @export
#'
#' @examples
#'
#' dat <- wgs84_to_unitM(cornersData, 28355)
#' dat_plots <- corners_to_plots(dat, 80, 24.2, 11, 20)
#' dat_plots_RR <- addRunRange(dat_plots, "BL")
#'
#'
#' moved_plots <- movePlotRanges(dat_plots_RR, "right", 0.8, c(1,3,5))
#'
#' plot(moved_plots)
#'
#'
movePlotRanges <- function(sf_obj, moveDirection, moveDistance, rangeSelection){

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

  return(new_sf)
}