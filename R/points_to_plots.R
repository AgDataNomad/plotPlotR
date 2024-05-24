#' Make plots from points i.e. plot centers or use to adjust length and the width of plots in an existing plot layout
#'
#' @param points_data A sf object with geometry type POINTS referring to plot center or POLYGONS
#' @param plot_length Length of individual plots
#' @param plot_width Width of individual plots
#'
#' @return A sf object of geometry type POLYGON with the same CRS as input points_data
#' @export
#'
#' @examples
#'
#' #reading a corner points data
#'
#' dat <- read_input_dat(cornersData, 4326)
#'
#' #Making a default plot grid using exp length and width, and using number of plots
#'
#' dat_plots <- corners_to_plots(dat, 80, 24.2, 11, 20)
#'
#' plot(dat_plots)
#'
#' #Adjusting the size of the individual plots
#'
#' adj_plots <- make_plots(dat_plots, 2.5, 1.2)
#'
#' plot(adj_plots)
#'
#'
make_plots <- function(points_data, plot_length, plot_width){

  expand_box_by <- plot_width/2

  points_data_c <- points_data %>%
    st_centroid()

  dgbb <- st_buffer(points_data_c,
                    dist = expand_box_by,
                    endCapStyle = "SQUARE")

  dgbb <- dgbb %>%
    st_coordinates() %>%
    as.data.frame.matrix()

  num_plots <- nrow(dgbb)/5

  l_in <- (plot_length - plot_width)/2

  y_adj <- rep(l_in * c(-1,1,1,-1), num_plots)

  ord_df <- data.frame(pos = c("BL","TL","TR","BR"),
                       ord = 1:4)

  dgbb_ord <- dgbb %>%
    group_by(L2) %>%
    distinct(X,Y) %>%
    arrange(Y) %>%
    mutate(p1 = c("B","B","T","T")) %>%
    arrange(X) %>%
    mutate(p2 = c("L", "L", "R", "R")) %>%
    mutate(pos = paste0(p1, p2)) %>%
    left_join(ord_df, by = "pos") %>%
    ungroup() %>%
    arrange(L2, ord) %>%
    select(L2, X, Y) %>%
    mutate(Y = Y + y_adj)

  dgbb_poly <- dgbb_ord %>%
    group_by(L2) %>%
    st_as_sf(coords = c("X", "Y")) %>%
    mutate(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON") %>%
    ungroup() %>%
    distinct_all() %>%
    select(-L2)

  print(rad_2_deg(rot_angle(points_data)))

  rot_poly <- rot_plots(dgbb_poly, rotation_angle = rad_2_deg(rot_angle(points_data)), selection = "plots")

  rot_poly <- cbind(st_as_sf(rot_poly), st_drop_geometry(points_data))

  st_crs(rot_poly) <- st_crs(points_data)

  st_geometry(rot_poly) <- "geometry"

  return(rot_poly)

}
