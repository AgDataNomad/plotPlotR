#' Make plots from points i.e. plot centers or use to adjust length and the width of plots in an existing plot layout
#'
#' @param sf_obj A sf object with geometry type POINTS referring to plot center or POLYGONS
#' @param plot_length Length of individual plots
#' @param plot_width Width of individual plots
#'
#' @return A sf object of geometry type POLYGON with the same CRS as input sf_obj
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
make_plots <- function(sf_obj, plot_length, plot_width){

  expand_box_by <- plot_width/2

  sf_obj_c <- sf_obj %>%
    st_centroid()

  dgbb <- st_buffer(sf_obj_c,
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
    distinct_all()

  rot_poly <- (st_geometry(dgbb_poly)-st_centroid(st_geometry(dgbb_poly)))*rot_shape(rot_angle(sf_obj))+st_centroid(st_geometry(dgbb_poly))

  rot_poly <- cbind(st_as_sf(rot_poly), st_drop_geometry(sf_obj))

  st_crs(rot_poly) <- st_crs(sf_obj)

  st_geometry(rot_poly) <- "geometry"

  return(rot_poly)

}
