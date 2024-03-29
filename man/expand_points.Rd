% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_buffer.R
\name{expand_points}
\alias{expand_points}
\title{Expand points}
\usage{
expand_points(sf_obj, expand_by)
}
\arguments{
\item{sf_obj}{A sf object of geometry type POINT. CRS needs to be in unit meters}

\item{expand_by}{A numeric value. Distance to expand the area in meters}
}
\value{
A sf object of geometry type POINT. CRS will be 4326
}
\description{
For the purposes of creating flight plan when you know the corners of your experiment
}
\examples{

dat <- read_input_dat(cornersData, 4326, 28355)

dat_expanded <- expand_points(dat, 5)

plot(dat)
plot(dat_expanded, add = TRUE)

}
