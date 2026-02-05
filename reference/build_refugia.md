# Build rasters of binary refugia

This function creates annual rasters of refugia for an ESM of interest,
using a temporal threshold (where refugia conditions must be met for 50
or 95% of the year). The function also displays a progress bar.

## Usage

``` r
build_refugia(
  percentdays = abalone::percentdays,
  esm = c("ens", "gfdltv", "hadtv", "ipsltv"),
  yrst = 1990,
  yrend = 2100,
  persist_thresh = c(50, 95),
  progress = TRUE,
  save_path = NULL
)
```

## Arguments

- percentdays:

  R object. Dataframe. Defaults to
  [`abalone::percentdays`](https://jessicabolin.github.io/abalone/reference/percentdays.md)
  (i.e., example package dataset of % days of stress)

- esm:

  Character. ESM model to use. Choose from `ens`, `gfdltv`, `hadtv`,
  `ipsltv`.

- yrst:

  Integer. Start year. Defaults to 1990.

- yrend:

  Integer. End year. Defaults to 2100.

- persist_thresh:

  Integer. Temporal threshold (%) used for defining annual refugia.
  Choose either 50 (liberal) or 95% (conservative).

- progress:

  Logical. Show progress bar? Default is TRUE.

- save_path:

  Character. Directory to save output to. Optional, set to NULL if not
  needed.

## Value

Produces a
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object of binary refugia (0/1), for the ESM specified, for all years
between `yrst` and `yrend`.

## Examples

``` r
ens_refugia <- build_refugia(percentdays = abalone::percentdays,
esm = "ens", yrst = 1990, yrend = 2100, persist_thresh = 50, progress = FALSE,
save_path = NULL)
ens_refugia
#> class       : SpatRaster 
#> size        : 286, 286, 111  (nrow, ncol, nlyr)
#> resolution  : 0.03321678, 0.03323263  (x, y)
#> extent      : -126, -116.5, 32.49849, 42.00302  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> names       : 1990, 1991, 1992, 1993, 1994, 1995, ... 
#> min values  :    0,    0,    0,    0,    0,    0, ... 
#> max values  :    0,    0,    1,    1,    0,    1, ... 
terra::plot(ens_refugia[[98]], main = "Refugia in 2088")

```
