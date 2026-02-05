# Map of persistence of refugia

This produces a nice `tmap` object that shows persistence of refugia (%)
for the time period of interest

## Usage

``` r
viz_persist(
  yr_range = 2070:2099,
  esm = c("gfdltv", "hadtv", "ipsltv", "ens"),
  area = c("monterey_bay", "channel_islands", "fort_bragg", "san_francisco"),
  def = "def8",
  extent_list = abalone::extent_list,
  breaks = seq(0, 100, 20),
  persist_thresh = c(50, 95)
)
```

## Arguments

- yr_range:

  Numeric vector range of years. Defaults to 2070:2099

- esm:

  ESM model. Choose one from `c("gfdltv", "hadtv", "ipsltv", "ens")`

- area:

  Character. Name of the area; must match a key in
  [`abalone::extent_list`](https://jessicabolin.github.io/abalone/reference/extent_list.md).

- def:

  Character. Refugia definition name (e.g., "def8") used in input file
  paths.

- extent_list:

  List of vectors. Defaults to
  [`abalone::extent_list`](https://jessicabolin.github.io/abalone/reference/extent_list.md)

- breaks:

  Sequence of integers. Defines the breaks used for the zlimits of the
  map. Defaults to `seq(0, 100, 20)`

- persist_thresh:

  Integer. Temporal threshold (%) used for defining annual refugia.
  Choose either 50 (liberal) or 95% (conservative).

## Value

Produces a customized `tmap` object of a map of persistence

## Examples

``` r
viz_persist(yr = 2070:2099, esm = "ens", area = "monterey_bay",
def = "def8", extent_list = abalone::extent_list, breaks = seq(0, 100, 20),
persist_thresh = 50)

```
