
<!-- README.md is generated from README.Rmd. Please edit that file -->

# plotPlotR

<!-- badges: start -->
<!-- badges: end -->

**plotPlotR** referring to plot plotter is a package containing set of
functions to create plot layouts on field trial as segments or
boundaries for the purposes of extracting value from each plot or adding
new data against each plot.

The functions are based on the implementation of “**sf**” package in R
and it provides users with a set of tools that are simple for R users to
create or manipulate plot boundaries. Users who may not be familiar with
spatial data or want functions that can be used easily, repeatedly and
in a automated way can use this package.

Agricultural research and farm management are increasingly becoming
digital. Equipment used in farm operations and data collection are often
connected to the internet, satellites and generates rich meta-data.
These meta-data has the potential to become key research support tools
that generates new opportunity.

**plotPlotR** takes GPS data from the connected devices or manually
collected coordinates from rasters or other sources to develop tools
that supports agricultural research and farm management. Since it is
spatial data we are talking about, the applications of this package
extend beyond agriculture to preserving biodiverstity, land management,
soil conservation etc.

## Installation

You can install the development version of plotPlotR from
[GitHub](https://github.com/AgDataNomad/plotPlotR) with:

``` r
# install.packages("devtools")
devtools::install_github("AgDataNomad/plotPlotR")
```

## Example

This is a simple example showing how to create plot layout using 4
corner points:

``` r
library(plotPlotR)
## A simple function to create plot layout with corner points from a csv file

cornersData # dataframe with 4 XY points corresponding to four corners of an experiment
#>          X         Y id
#> 1 148.6870 -34.47064  1
#> 2 148.6873 -34.47062  4
#> 3 148.6869 -34.46992  2
#> 4 148.6872 -34.46990  3
```

``` r

dat <- read_input_dat(cornersData, 4326, 28355) # Reading data and converting to unit M CRS

dat_plots <- corners_to_plots(dat, 80, 24.2, 11, 20)
#> [1] "make sure the orientation is correct!!"
```

``` r

dat_plots <- addRunRange(dat_plots, "BL", n_runs = 11, n_ranges = 20)

plot(dat_plots)
```

<img src="man/figures/README-example-1.png" width="100%" />
