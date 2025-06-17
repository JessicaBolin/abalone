#' Map of persistence of refugia
#'
#' This produces a nice `tmap` object that shows persistence of refugia (%) for the time period of interest
#'
#' @param yr_range Numeric vector range of years. Defaults to 2070:2099
#' @param model ESM model. Choose one from `c("gfdltv", "hadtv", "ipsltv", "ens")`
#' @param area Character. Name of the area; must match a key in `abalone::extent_list`.
#' @param def Character. Refugia definition name (e.g., "def8") used in input file paths.
#' @param extents List of vectors. Defaults to `abalone::extent_list`
#' @param breaks Sequence of integers. Defines the breaks used for the zlimits of the map. Defaults to `seq(0, 100, 20)`
#' @param persist_thresh Integer. Temporal threshold (%) used for defining annual refugia. Choose either 50 (liberal) or 95% (conservative).
#'
#' @return Produces a customized `tmap` object of a map of persistence
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' viz_persist(yr = 2070:2099, model = "ens", area = "monterey_bay",
#' def = "def8", extents = abalone::extent_list, breaks = seq(0, 100, 20),
#' persist_thresh = 50)
#'
viz_persist <- function(yr_range = 2070:2099,
                        model = c("gfdltv", "hadtv", "ipsltv", "ens"),
                        area = c("monterey_bay", "channel_islands", "fort_bragg", "san_francisco"),
                        def = "def8",
                        extents = abalone::extent_list,
                        breaks = seq(0, 100, 20),
                        persist_thresh = c(50, 95)) {
  # Temporarily suppress tmap messages
  old_opt <- options(tmap.messages = FALSE)
  on.exit(options(old_opt), add = TRUE)
  tmap::tmap_options(show.messages = FALSE, component.autoscale = FALSE)

  model2 <- if (model == "^zoom") "zoom" else model

  # filey <- file.path(usethis::proj_path(), "inst", "extdata",
  #                    paste0(model2, "_persistence_", persist_thresh, "_",
  #                           min(yr_range), "-", max(yr_range), ".tif"))

  # System.file() works instead fo file.path, because the latter only works
  # in the dev enviornment only. Inst directory deletes when running r CMD check.
  filey <- system.file("extdata",
                       paste0(model2, "_persistence_", persist_thresh, "_",
                              min(yr_range), "-", max(yr_range), ".tif"),
                       package = "abalone")

  # Load shapefiles within each worker
  eez <- read_shp(shape = "eez")
  ca <- read_shp("usa")
  base_rast <- cali_rast() %>%
    terra::project("EPSG:4326")

  r <- terra::rast(filey)

  # Crop
  if (area %in% names(extents)) {
    r <- terra::crop(r, terra::ext(extents[[area]]))
  }

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
    tmap::tm_raster(col.scale = tmap::tm_scale(
        breaks = breaks,
        values = viridis::magma(255)),
      col.legend = tmap::tm_legend(title = "% persist.",
                                   position = c("right", "top"),
                                   text.size = 0.9,
                                   frame = TRUE,
                                   frame.lwd = 0.001,
                                   title.size = 1.1,
                                   bg.alpha = 0,
                                   width = 6,
                                   height = 11)) +
    tmap::tm_shape(ca) +
    tmap::tm_polygons(fill_alpha = 0.1) +
    tmap::tm_title(paste0(def %>% toupper(), " | ",
                          model2, " | ", min(yr_range), "-", max(yr_range),
                          " | % persist.", " | ", persist_thresh, "%")) +
    tmap::tm_scalebar(position = c("left", "bottom"),
                      text.size = 0.7) +
    tmap::tm_layout(
      legend.title.size = 1.4,
      bg.color = "white",
      legend.frame = FALSE)

  return(tt)


}

