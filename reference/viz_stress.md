# Map of % of year stressed

This produces a nice `tmap` object that shows the % of year stressful
conditions were experienced by abalone.

## Usage

``` r
viz_stress(
  yr = 2100,
  esm = c("gfdltv", "hadtv", "ipsltv", "ens"),
  area = c("monterey_bay", "channel_islands", "fort_bragg", "san_francisco"),
  def = "def8",
  extent_list = abalone::extent_list,
  infile = abalone::percentdays
)
```

## Arguments

- yr:

  Numeric vector. Year to plot. Default is 2100.

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

- infile:

  Input dataframe detailing % stress per year. Defaults to
  [`abalone::percentdays`](https://jessicabolin.github.io/abalone/reference/percentdays.md).

## Value

Produces a customized `tmap` object of a map of stress for the year of
interest

## Examples

``` r
viz_stress(yr = 2098, esm = "gfdltv", area = "monterey_bay",
def = "def8", extent_list = abalone::extent_list,
infile = abalone::percentdays)

```
