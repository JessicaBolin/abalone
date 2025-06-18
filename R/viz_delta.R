#' Map of delta refugia
#'
#' This produces a nice `tmap` object that shows delta refugia (%) for the time period of interest
#'
#' @param esm ESM model. Choose one from `c("gfdltv", "hadtv", "ipsltv", "ens")`
#' @param area Character. Name of the area; must match a key in `abalone::extent_list`.
#' @param def Character. Refugia definition name (e.g., "def8") used in input file paths.
#' @param extent_list List of vectors. Defaults to `abalone::extent_list`
#' @param hist_range Vector of integers. Time period for historical period
#' @param proj_range Vector of integers. Time period for projection period
#' @param persist_thresh Integer. Temporal threshold (%) used for defining annual refugia. Choose either 50 (liberal) or 95% (conservative).
#' @param save_path Character. Directory to save output to. Optional, set to NULL if not needed.
#'
#' @return Produces a customized `tmap` object of a map of change in refugia
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom grDevices colorRampPalette

#'
#' @examples
#' viz_delta(esm = "ens", area = "monterey_bay", def = "def8",
#' hist_range = 2020:2049, proj_range = 2070:2099,
#' extent_list = abalone::extent_list, persist_thresh = 50, save_path = NULL)
#'
viz_delta <- function(esm = c("gfdltv", "hadtv", "ipsltv", "ens"),
                      area = c("monterey_bay", "channel_islands", "fort_bragg",
                               "san_francisco"),
                      def = "def8",
                      hist_range = 2020:2049,
                      proj_range = 2070:2099,
                      extent_list = abalone::extent_list,
                      persist_thresh = c(50, 95),
                      save_path = NULL) {

  # Temporarily suppress tmap messages
  old_opt <- options(tmap.messages = FALSE)
  on.exit(options(old_opt), add = TRUE)
  tmap::tmap_options(show.messages = FALSE, component.autoscale = FALSE)

  model2 <- if (esm == "^zoom") "zoom" else esm

  filey <- system.file("extdata",
                       paste0(save_path, "/", model2, "_delta_", persist_thresh,
                              "_", min(proj_range), "-", max(proj_range), "minus",
                              min(hist_range), "-", max(hist_range), ".tif"),
                       package = "abalone")

  # Load shapefiles within each worker
  eez <- read_shp(shape = "eez")
  ca <- read_shp("usa")
  base_rast <- cali_rast() %>%
    terra::project("EPSG:4326")

  r <- terra::rast(filey)

  # Crop
  if (area %in% names(extent_list)) {
    r <- terra::crop(r, terra::ext(extent_list[[area]]))
  }

  vals <- c(terra::global(r, "min", na.rm = TRUE)[[1]],
            terra::global(r, "max", na.rm = TRUE)[[1]])
  max_abs <- max(abs(vals))  # symmetric around zero
  cols <- colorRampPalette(c("red", "white", "blue"))(100)

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
      values = "brewer.rd_bu", midpoint = 0, breaks = seq(-100, 100, by = 25)),
      col.legend = tmap::tm_legend(title = "% Delta",
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
                          model2, " | ",
                          min(proj_range), "-", max(proj_range), "-",
                          min(hist_range), "-", max(hist_range), " | ",
                          persist_thresh, "%")) +
    tmap::tm_scalebar(position = c("left", "bottom"),
                      text.size = 0.7) +
    tmap::tm_layout(
      legend.title.size = 1.4,
      bg.color = "white",
      legend.frame = FALSE)

  return(tt)

}
