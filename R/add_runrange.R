#' Add Run and Range values to a field experiment plots
#'
#' After generating plot boundaries, this functions allows users to assign Run and Range values. To identify run1_range1 value assume you are on the Southern end of your experiment width, facing Northern end. Bottom is close to you, Top is away from you. Left on your left side and Right on your right side.
#'
#' @param sf_object A sf object of geometry type POLYGON or MULTIPOLYGON
#' @param run1_range1 Position of Run 1 and Range 1. Will take 4 different options. "TR","TL","BR","BL" denoting top right, top left, bottom right, bottom left respectively.
#'
#' @return A sf object with Run and Range columns added.
#' @export
#'
#' @examples
#'
#' a <- corners_to_plots(sf_object = cornersData, exp_length = 80, exp_width = 24.2, n_runs = 11, n_ranges = 20)
#'
#' a_w_RunRange <- addRunRange(a, "BL")
#'
#'
addRunRange <- function(sf_object, run1_range1){

  options(digits = 15)

  ra <- rot_angle(sf_object)

  centrd <- st_centroid(st_geometry(sf_object))

  centrd <- st_centroid(st_union(centrd))

  poly_straight <- (st_as_sfc(sf_object) - centrd) * rot_shape((360*pi/180)-ra) + centrd

  poly_straight <- st_centroid(poly_straight)

  poly_centers_df <- as.data.frame(st_coordinates(poly_straight))

  plots_along_width <- length(unique(substring(poly_centers_df$X, 1, 10)))

  plots_along_length <- length(unique(substring(poly_centers_df$Y, 1, 10)))

  poly_centers_df$X1 <- as.numeric(substring(poly_centers_df$X, 1, 10))
  poly_centers_df$Y1 <- as.numeric(substring(poly_centers_df$Y, 1, 11))

    if (run1_range1 == "TR") {
    poly_centers_df <- poly_centers_df %>%
      mutate(id = 1:n()) %>%
      arrange(Y1) %>%
      mutate(range = rep(plots_along_length:1, each = plots_along_width)) %>%
      arrange(range, X1) %>%
      mutate(run = rep(plots_along_width:1, plots_along_length)) %>%
      arrange(id) %>%
      select(-id)
    } else if (run1_range1 == "TL") {
      poly_centers_df <- poly_centers_df %>%
        mutate(id = 1:n()) %>%
        arrange(Y1) %>%
        mutate(range = rep(plots_along_length:1, each = plots_along_width)) %>%
        arrange(range, X1) %>%
        mutate(run = rep(1:plots_along_width, plots_along_length)) %>%
        arrange(id) %>%
        select(-id)
    } else if (run1_range1 == "BL") {
      poly_centers_df <- poly_centers_df %>%
        mutate(id = 1:n()) %>%
        arrange(Y1) %>%
        mutate(range = rep(1:plots_along_length, each = plots_along_width)) %>%
        arrange(range, X1) %>%
        mutate(run = rep(1:plots_along_width, plots_along_length)) %>%
        arrange(id) %>%
        select(-id)
    } else if (run1_range1 == "BR") {
      poly_centers_df <- poly_centers_df %>%
        mutate(id = 1:n()) %>%
        arrange(Y1) %>%
        mutate(range = rep(1:plots_along_length, each = plots_along_width)) %>%
        arrange(range, X1) %>%
        mutate(run = rep(plots_along_width:1, plots_along_length)) %>%
        arrange(id) %>%
        select(-id)
    }

  sf_object <- sf_object %>%
    mutate(Run = poly_centers_df$run,
           Range = poly_centers_df$range)

  return(sf_object)
}
