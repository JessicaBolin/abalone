# Example dataset of % days of stress

For refugia definition 8: growth, defined as when temperatures are
between 16-19˚C.

## Usage

``` r
percentdays
```

## Format

A data frame with 6 columns, and 697524 rows:

- cellID:

  cellID corresponding to the cell index position of the raster of
  California cropped to 100m isobath

- model:

  Earth System Model. Either "gfdltv", "hadtv", "ipsltv" or "ens"

- refugiadays:

  Number of days classified as refugia for that year

- percent:

  As above, but classed as a percentage value

- year:

  Year of interest

## Examples

``` r
head(percentdays)
#>   cellID refugiadays percent year model
#> 1     49           0       0 1990   ens
#> 2     50           0       0 1990   ens
#> 3     51           0       0 1990   ens
#> 4     52           0       0 1990   ens
#> 5     53           0       0 1990   ens
#> 6     54           0       0 1990   ens
str(percentdays)
#> 'data.frame':    697524 obs. of  5 variables:
#>  $ cellID     : num  49 50 51 52 53 54 335 336 337 338 ...
#>  $ refugiadays: num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ percent    : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ year       : num  1990 1990 1990 1990 1990 1990 1990 1990 1990 1990 ...
#>  $ model      : chr  "ens" "ens" "ens" "ens" ...
```
