% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ts_viz_percentdays.R
\name{ts_viz_percentdays}
\alias{ts_viz_percentdays}
\title{Plot percent of days meeting refugia conditions over time}
\usage{
ts_viz_percentdays(
  area = c("monterey_bay", "channel_islands", "fort_bragg", "san_francisco"),
  yr_range = 1990:2100,
  def = "def8",
  input_file = abalone::percentdays,
  cons_thresh = 95,
  lib_thresh = 50,
  extent_list = abalone::extent_list,
  plot_title = "\% of year refugia conditions met"
)
}
\arguments{
\item{area}{Character. Name of the area; must match a key in \code{abalone::extent_list}.}

\item{yr_range}{Numeric vector. Range of years to include. Default is 1990–2100.}

\item{def}{Character. Refugia definition name (e.g., "def8") used in input file paths.}

\item{input_file}{R Object. Dataframe of \% of year stressed. Defaults to \code{abalone::percentdays}}

\item{cons_thresh}{Integer. Value representing the conservative temporal threshold to define refugia. Defaults to 95(\%)}

\item{lib_thresh}{Integer. Value representing the liberal temporal threshold to define refugia. Defaults to 50(\%)}

\item{extent_list}{List of vectors. Defaults to \code{abalone::extent_list}}

\item{plot_title}{Character string. Title displayed on the \code{ggplot}}
}
\value{
Produces a \code{ggplot2::ggplot} object of an annual time series of refugia.
}
\description{
This function produces a \code{ggplot2} object of an annual time series of refugia
across all ESMs, including the ensemble mean. The plot includes dashed horizontal
lines for the two temporal thresholds used to define refugia: 95 and 50\%. Note
this function will only work for refugia definitions that are calculated using
\% of year defined as refugia (i.e., not Definitions 1-2).
}
\examples{
ts_viz_percentdays(area = "monterey_bay", yr_range = 1990:2100, def = "def8",
input_file = abalone::percentdays, cons_thresh = 95, lib_thresh = 50)
}
