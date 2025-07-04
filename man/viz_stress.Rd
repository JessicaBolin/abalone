% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/viz_stress.R
\name{viz_stress}
\alias{viz_stress}
\title{Map of \% of year stressed}
\usage{
viz_stress(
  yr = 2100,
  esm = c("gfdltv", "hadtv", "ipsltv", "ens"),
  area = c("monterey_bay", "channel_islands", "fort_bragg", "san_francisco"),
  def = "def8",
  extent_list = abalone::extent_list,
  infile = abalone::percentdays
)
}
\arguments{
\item{yr}{Numeric vector. Year to plot. Default is 2100.}

\item{esm}{ESM model. Choose one from \code{c("gfdltv", "hadtv", "ipsltv", "ens")}}

\item{area}{Character. Name of the area; must match a key in \code{abalone::extent_list}.}

\item{def}{Character. Refugia definition name (e.g., "def8") used in input file paths.}

\item{extent_list}{List of vectors. Defaults to \code{abalone::extent_list}}

\item{infile}{Input dataframe detailing \% stress per year. Defaults to \code{abalone::percentdays}.}
}
\value{
Produces a customized \code{tmap} object of a map of stress for the year of interest
}
\description{
This produces a nice \code{tmap} object that shows the \% of year stressful conditions were experienced by abalone.
}
\examples{
viz_stress(yr = 2098, esm = "gfdltv", area = "monterey_bay",
def = "def8", extent_list = abalone::extent_list,
infile = abalone::percentdays)

}
