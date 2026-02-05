# ROMS 3km California subsetted to 100m depth

This is a
[`terra::rast`](https://rspatial.github.io/terra/reference/rast.html)
object of the California coast, subsetted to only include cells above
100 m depth.

## Usage

``` r
cali_rast(
  pth = system.file("extdata", "emptyrast_100.tif", package = "abalone")
)
```

## Source

Custom defined by project team

## Arguments

- pth:

  Internal package file path to where the `.tif` is stored

## Examples

``` r
emptyrast_100 <- cali_rast()
emptyrast_100
#> class       : SpatRaster 
#> size        : 286, 286, 1  (nrow, ncol, nlyr)
#> resolution  : 0.03321678, 0.03323263  (x, y)
#> extent      : -126, -116.5, 32.49849, 42.00302  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source      : emptyrast_100.tif 
#> name        : empty_roms_cali_100m 
#> min value   :                    1 
#> max value   :                    1 
#> time (days) : 1990-01-01 
terra::plot(emptyrast_100)

```
