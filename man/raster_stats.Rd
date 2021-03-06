% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tam-dm.R
\name{raster_stats}
\alias{raster_stats}
\title{Derive raster statistics: "median","mean", "min", "max", "sd", "skew"}
\usage{
raster_stats(i, .ls, .lines, .stat)
}
\arguments{
\item{i}{numeric, indice}

\item{.ls}{a RasterStack of terrain analysis rasters, passed from \code{get_stats_df()}}

\item{.lines}{a SpatialLinesDataFrame, passed from \code{get_stats_df()}}

\item{.stat}{character, list of statistics to derive, passed from \code{get_stats_df()}}
}
\value{
a numeric vector of statistics
}
\description{
Derive raster statistics: "median","mean", "min", "max", "sd", "skew"
}
\keyword{tam-dm}
