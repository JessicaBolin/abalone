# Build rasters of persistent refugia

This function creates a single raster of persistent refugia for an ESM
of interest, over a user-specified time period.

## Usage

``` r
build_delta(
  persist_thresh = c(50, 95),
  esm = c("ens", "gfdltv", "hadtv", "ipsltv"),
  def = "def8",
  hist_range = 2020:2049,
  proj_range = 2070:2099,
  save_path = NULL
)
```

## Arguments

- persist_thresh:

  Integer. Temporal threshold (%) used for defining annual refugia.
  Choose either 50 (liberal) or 95% (conservative).

- esm:

  Character. ESM model to use. Choose from `ens`, `gfdltv`, `hadtv`,
  `ipsltv`.

- def:

  Character. Refugia definition name (e.g., "def8") used in input file
  paths.

- hist_range:

  Vector of integers. Time period for historical period

- proj_range:

  Vector of integers. Time period for projection period

- save_path:

  Character. Directory to save output to. Optional, set to NULL if not
  needed.

## Value

Produces a
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object of delta refugia (%), for the ESM specified.

## Examples

``` r
deltar <- build_delta(persist_thresh = 50, esm = "ens",
hist_range = 2020:2049, proj_range = 2070:2099, save_path = NULL)
deltar
#> class       : SpatRaster 
#> size        : 286, 286, 1  (nrow, ncol, nlyr)
#> resolution  : 0.03321678, 0.03323263  (x, y)
#> extent      : -126, -116.5, 32.49849, 42.00302  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> varname     : ens_persistence_50_2070-2099 
#> name        : 2070-2099 
#> min value   : -56.66666 
#> max value   : 100.00000 
terra::plot(deltar)
```
