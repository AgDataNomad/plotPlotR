#' Add Run and Range values to a field experiment plots
#'
#' After generating plot boundaries, this functions allows users to assign Run and Range values. To identify what (where) is run1_range1 value,
#' assume you are standing on the **Southern end** of your **experiment width** side i.e. `exp_width` input in `corners_to_plot` function,
#' facing Northern end. Bottom is close to you, Top is away from you. Left on your left side and Right on your right side.
#'
#' @param sf_object A sf object of geometry type POLYGON or MULTIPOLYGON
#' @param run1_range1 Position of Run 1 and Range 1. Will take 4 different options. "TR","TL","BR","BL" denoting top right, top left, bottom right, bottom left respectively.
#' @param n_runs Numeric value. Number of runs in the experiment
#' @param n_ranges Numeric value. Number of ranges in the experiment
#'
#' @return A sf object with Run and Range columns added.
#' @export
#'
#' @examples
#'
#' dat <- read_input_dat(cornersData, 4326)
#'
#' dat_plot <- corners_to_plots(sf_object = dat,
#'                       exp_length = 80, exp_width = 24.2,
#'                       n_runs = 11, n_ranges = 20)
#'
#' datplot_w_RR <- addRunRange(dat_plot, "BL", 11, 20)
#'
#' plot(datplot_w_RR)
#'
#'
addRunRange <- function(sf_object, run1_range1, n_runs, n_ranges){

  options(digits = 15)

  ra <- rot_angle(sf_object)

  sf_obj <- sf_object %>%
    select(geometry) %>%
    distinct_all()

  centrd <- st_centroid(st_geometry(sf_obj))

  centrd <- st_centroid(st_union(centrd))

  poly_straight <- (st_as_sfc(sf_obj) - centrd) * rot_shape((360*pi/180)-ra) + centrd

  poly_straight <- st_centroid(poly_straight)

  poly_centers_df <- as.data.frame(st_coordinates(poly_straight))

  poly_centers_df$X1 <- round(poly_centers_df$X, 2)
  poly_centers_df$Y1 <- round(poly_centers_df$Y, 2)

    if (run1_range1 == "TR") {
    poly_centers_df <- poly_centers_df %>%
      mutate(id = 1:n()) %>%
      arrange(Y1) %>%
      mutate(range = rep(n_ranges:1, each = n_runs)) %>%
      arrange(range, X1) %>%
      mutate(run = rep(n_runs:1, n_ranges)) %>%
      arrange(id) %>%
      select(-id)
    } else if (run1_range1 == "TL") {
      poly_centers_df <- poly_centers_df %>%
        mutate(id = 1:n()) %>%
        arrange(Y1) %>%
        mutate(range = rep(n_ranges:1, each = n_runs)) %>%
        arrange(range, X1) %>%
        mutate(run = rep(1:n_runs, n_ranges)) %>%
        arrange(id) %>%
        select(-id)
    } else if (run1_range1 == "BL") {
      poly_centers_df <- poly_centers_df %>%
        mutate(id = 1:n()) %>%
        arrange(Y1) %>%
        mutate(range = rep(1:n_ranges, each = n_runs)) %>%
        arrange(range, X1) %>%
        mutate(run = rep(1:n_runs, n_ranges)) %>%
        arrange(id) %>%
        select(-id)
    } else if (run1_range1 == "BR") {
      poly_centers_df <- poly_centers_df %>%
        mutate(id = 1:n()) %>%
        arrange(Y1) %>%
        mutate(range = rep(1:n_ranges, each = n_runs)) %>%
        arrange(range, X1) %>%
        mutate(run = rep(n_runs:1, n_ranges)) %>%
        arrange(id) %>%
        select(-id)
    }

  if (nrow(sf_object)==nrow(sf_obj)) {
    sf_object <- sf_object %>%
      mutate(Run = poly_centers_df$run,
             Range = poly_centers_df$range)
  } else {
    sf_object <- sf_obj %>%
      mutate(Run = poly_centers_df$run,
             Range = poly_centers_df$range)
    }

  return(sf_object)
}
