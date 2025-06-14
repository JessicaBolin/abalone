#' Map of % of year stressed
#'
#' This produces a nice `tmap` object that shows the % of year stressful conditions were experienced by abalone. Note that this code uses `tmap` Version 3 code syntax.
#'
#' @param yr Numeric vector. Year to plot. Default is 2100.
#' @param model ESM model. Choose one from `c("gfdltv", "hadtv", "ipsltv", "ens")`
#' @param area Character. Name of the area; must match a key in `abalone::extent_list`.
#' @param def Character. Refugia definition name (e.g., "def8") used in input file paths.
#' @param extents List of vectors. Defaults to `abalone::extent_list`
#' @param infile Input dataframe detailing % stress per year. Defaults to `abalone::percentdays`.
#'
#' @return Produces a customized `tmap` object of a map of stress for the year of interest
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' suppressWarnings(suppressMessages(
#' viz_stress_map(yr = 2098, model = "gfdltv", area = "monterey_bay",
#' def = "def8", extents = abalone::extent_list, infile = abalone::percentdays)))
#'

viz_stress_map <- function(yr = 2100,
                      model = c("gfdltv", "hadtv", "ipsltv", "ens"),
                      area = c("monterey_bay", "channel_islands", "fort_bragg", "san_francisco"),
                      def = "def8",
                      extents = abalone::extent_list,
                      infile = abalone::percentdays
                      ) {

  tmap::tmap_options(show.messages = F)
  model2 <- if (model == "^zoom") "zoom" else model

  # Load shapefiles within each worker
  eez <- read_shp(shape = "eez")
  ca <- read_shp("usa")
  base_rast <- cali_rast() %>%
    terra::project("EPSG:4326")

  r <- infile %>%
    dplyr::filter(model == model2) %>%
    dplyr::filter(year == yr)

  base_rast[terra::cells(base_rast)] <- r$percent
 # terra::plot(base_rast)

  # Crop
  if (area %in% names(extents)) {
    r <- terra::crop(base_rast, terra::ext(extents[[area]]))
  }

    my_breaks <- seq(0, 100, by = 10)

  # Map
  tt <- tmap::tm_shape(r) +
    tmap::tm_raster(style = "cont",
                    palette = "white",
                    legend.show = FALSE) +
    tmap::tm_shape(eez) +
    tmap::tm_polygons(col = "white") +
    tmap::tm_graticules(ticks = TRUE,
                        lwd = 0.5,
                        col = "grey50",
                        labels.size = 1,
                        n.y = 6) +
    tmap::tm_shape(ca) +
    tmap::tm_polygons() +
    tmap::tm_shape(r) +
    tmap::tm_raster(style = "cont",
              palette = viridis::magma(255),
              breaks = my_breaks, title = "%") +
    tmap::tm_shape(ca) +
    tmap::tm_polygons(alpha = 0.1) +
    tmap::tm_legend(position = c("right", "top"),
                    legend.text.size = 1,
              frame = TRUE,
              frame.lwd = 0.001) +
    tmap::tm_layout(main.title = paste0(def %>% toupper(), " | ",
                                  model2, " | ", yr, " | % yr stressed"),
              legend.title.size = 1.4,
              bg.color = "white",
              legend.frame = FALSE) +
    tmap::tm_scale_bar(position = "left",
                       text.size = 0.8)

  return(tt)
}


# library(tmap)
# library(tidyverse)
# yr = 1990
# model = "gfdltv"
# area = "monterey_bay"
# def = "def8"
# extents = extent_list
# infile = abalone::percentdays
