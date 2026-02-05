# Build rasters of persistent refugia

This function creates a single raster of persistent refugia for an ESM
of interest, over a user-specified time period.

## Usage

``` r
build_persist(
  esm = c("ens", "gfdltv", "hadtv", "ipsltv"),
  yr_range = c(1990:2019),
  persist_thresh = c(50, 95),
  save_path = NULL
)
```

## Arguments

- esm:

  Character. ESM model to use. Choose from `ens`, `gfdltv`, `hadtv`,
  `ipsltv`.

- yr_range:

  Vector of integers. Time period.

- persist_thresh:

  Integer. Temporal threshold (%) used for defining annual refugia.
  Choose either 50 (liberal) or 95% (conservative).

- save_path:

  Character. Directory to save output to. Optional, set to NULL if not
  needed.

## Value

Produces a
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object of persistent refugia (%), for the ESM specified.

## Examples

``` r
persist_refugia <- build_persist(esm = "ens", yr_range = 2070:2099,
persist_thresh = 50, save_path = NULL)
persist_refugia
#> class       : SpatRaster 
#> size        : 286, 286, 1  (nrow, ncol, nlyr)
#> resolution  : 0.03321678, 0.03323263  (x, y)
#> extent      : -126, -116.5, 32.49849, 42.00302  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> name        : 2070-2099 
#> min value   :         0 
#> max value   :       100 
terra::plot(persist_refugia)
```
