#' Map of % of year stressed
#'
#' This produces a nice `tmap` object that shows the % of year stressful conditions were experienced by abalone.
#'
#' @param yr Numeric vector. Year to plot. Default is 2100.
#' @param esm ESM model. Choose one from `c("gfdltv", "hadtv", "ipsltv", "ens")`
#' @param area Character. Name of the area; must match a key in `abalone::extent_list`.
#' @param def Character. Refugia definition name (e.g., "def8") used in input file paths.
#' @param extent_list List of vectors. Defaults to `abalone::extent_list`
#' @param infile Input dataframe detailing % stress per year. Defaults to `abalone::percentdays`.
#'
#' @return Produces a customized `tmap` object of a map of stress for the year of interest
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' viz_stress(yr = 2098, esm = "gfdltv", area = "monterey_bay",
#' def = "def8", extent_list = abalone::extent_list,
#' infile = abalone::percentdays)
#'

viz_stress <- function(yr = 2100,
                       esm = c("gfdltv", "hadtv", "ipsltv", "ens"),
                      area = c("monterey_bay", "channel_islands",
                               "fort_bragg", "san_francisco"),
                      def = "def8",
                      extent_list = abalone::extent_list,
                      infile = abalone::percentdays
                      ) {

  # Temporarily suppress tmap messages
  old_opt <- options(tmap.messages = FALSE)
  on.exit(options(old_opt), add = TRUE)

  tmap::tmap_options(show.messages = FALSE, component.autoscale = FALSE)

  model2 <- if (esm == "^zoom") "zoom" else esm

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
  if (area %in% names(extent_list)) {
    r <- terra::crop(base_rast, terra::ext(extent_list[[area]]))
  }

    my_breaks <- seq(0, 100, by = 10)

  # Map
  tt <- tmap::tm_shape(r) +
    tmap::tm_raster(col.scale = tmap::tm_scale_continuous(),
                    col = "white",
                    col.legend = tmap::tm_legend_hide()) +
    tmap::tm_graticules(ticks = TRUE,
                        lwd = 0.5,
                        col = "grey50",
                        labels.size = 1,
                        n.y = 6) +
    tmap::tm_shape(ca) +
    tmap::tm_polygons() +
    tmap::tm_shape(r) +
      tmap::tm_raster(
        col.scale = tmap::tm_scale(
          breaks = my_breaks,
          values = viridis::magma(255)),
        col.legend = tmap::tm_legend(title = "% of year",
                                     position = c("right", "top"),
                                     text.size = 0.9,
                                     frame = TRUE,
                                     frame.lwd = 0.001,
                                     title.size = 1.1,
                                     bg.alpha = 0,
                                     width = 6,
                                     height = 11)
      ) +
    tmap::tm_shape(ca) +
    tmap::tm_polygons(fill_alpha = 0.1) +
    tmap::tm_title(paste0(def %>% toupper(), " | ",
                          model2, " | ", yr, " | % yr stressed")) +
    tmap::tm_scalebar(position = c("left", "bottom"),
                      text.size = 0.7) +
    tmap::tm_layout(
              legend.title.size = 1.4,
              bg.color = "white",
              legend.frame = FALSE)

  return(tt)
}


# #
# yr = 2089
# esm = "gfdltv"
# area = "monterey_bay"
# def = "def8"
# extent_list = extent_list
# infile = abalone::percentdays
