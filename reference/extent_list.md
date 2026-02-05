# List of extents for case study regions

This object contains bounding boxes (latitudes and longitudes) for key
study regions used in the refugia analysis. These can be used with
[`terra::ext()`](https://rspatial.github.io/terra/reference/ext.html)
for vizualisation.

## Usage

``` r
extent_list
```

## Format

An object of class `list` of length 4.

## Source

Custom defined by project team

## Details

**Available extents:**

- **Monterey Bay**: `monterey_bay`

- **Fort Bragg**: `fort_bragg`

- **Channel Islands**: `channel_islands`

- **San Francisco**: `san_francisco`

## Examples

``` r
extent_list
#> $monterey_bay
#>   xmin   xmax   ymin   ymax 
#> -122.3 -121.5   36.4   37.1 
#> 
#> $fort_bragg
#>   xmin   xmax   ymin   ymax 
#> -124.3 -123.5   39.1   39.7 
#> 
#> $channel_islands
#>    xmin    xmax    ymin    ymax 
#> -120.50 -119.40   33.80   34.15 
#> 
#> $san_francisco
#>   xmin   xmax   ymin   ymax 
#> -123.5 -121.9   37.3   38.5 
#> 
```
