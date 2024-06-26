% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/collect_corners.R
\name{collect_corners}
\alias{collect_corners}
\title{Collect corners of a raster}
\usage{
collect_corners(raster_object, numPoints = NULL)
}
\arguments{
\item{raster_object}{A raster object or path to raster object}

\item{numPoints}{Integer value. Has to be multiples of 4. Each 4 points should have the same center as the experiment.}
}
\value{
A sf object with corners of experiment from a raster
}
\description{
This will temporarily load raster in your environment
Expect this process to be slow
}
