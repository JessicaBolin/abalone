# Package index

## Create rasters

Functions used to define and create refugia rasters. These should be run
in descending order.

- [`build_stress()`](https://jessicabolin.github.io/abalone/reference/build_stress.md)
  : Create annual stress % rasters
- [`build_refugia()`](https://jessicabolin.github.io/abalone/reference/build_refugia.md)
  : Build rasters of binary refugia
- [`build_persist()`](https://jessicabolin.github.io/abalone/reference/build_persist.md)
  : Build rasters of persistent refugia
- [`build_delta()`](https://jessicabolin.github.io/abalone/reference/build_delta.md)
  : Build rasters of persistent refugia

## Vizualise refugia

Functions used to map refugia with `tmap`.

- [`viz_stress()`](https://jessicabolin.github.io/abalone/reference/viz_stress.md)
  : Map of % of year stressed
- [`viz_refugia()`](https://jessicabolin.github.io/abalone/reference/viz_refugia.md)
  : Map of refugia per year
- [`viz_persist()`](https://jessicabolin.github.io/abalone/reference/viz_persist.md)
  : Map of persistence of refugia
- [`viz_delta()`](https://jessicabolin.github.io/abalone/reference/viz_delta.md)
  : Map of delta refugia

## Time series plots of refugia

Functions used to map annual time series of refugia

- [`ts_viz_percentdays()`](https://jessicabolin.github.io/abalone/reference/ts_viz_percentdays.md)
  : Plot percent of days meeting refugia conditions over time
- [`ts_viz_refugia()`](https://jessicabolin.github.io/abalone/reference/ts_viz_refugia.md)
  : Plot proportion of year meeting refugia conditions over time

## Misc. functions

These are miscellaneous functions.

- [`read_shp()`](https://jessicabolin.github.io/abalone/reference/read_shp.md)
  : Read in shapefiles provided by this package

## Datasets

These are internal datasets specific to our project, and are required
for running package examples.

- [`extent_list`](https://jessicabolin.github.io/abalone/reference/extent_list.md)
  : List of extents for case study regions
- [`percentdays`](https://jessicabolin.github.io/abalone/reference/percentdays.md)
  : Example dataset of % days of stress
- [`cali_rast()`](https://jessicabolin.github.io/abalone/reference/cali_rast.md)
  : ROMS 3km California subsetted to 100m depth
