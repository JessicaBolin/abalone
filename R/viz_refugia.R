#' Map of refugia per year
#'
#' This produces a nice `tmap` object that shows binary refugia for the year of interest
#'
#' @param yr Numeric vector. Year to plot. Default is 2100.
#' @param model ESM model. Choose one from `c("gfdltv", "hadtv", "ipsltv", "ens")`
#' @param area Character. Name of the area; must match a key in `abalone::extent_list`.
#' @param def Character. Refugia definition name (e.g., "def8") used in input file paths.
#' @param extents List of vectors. Defaults to `abalone::extent_list`
#' @param infile Input dataframe detailing % stress per year. Defaults to `abalone::percentdays`.
#' @param thresh Integer. Temporal threshold (%) used for defining annual refugia. Choose either 50 (liberal) or 95% (conservative).

#' @return Produces a customized `tmap` object of a map of refugia for the year of interest
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' viz_refugia(yr = 2098, model = "gfdltv", area = "monterey_bay",
#' def = "def8", extents = abalone::extent_list, infile = abalone::percentdays,
#' thresh = 50)
#'


viz_refugia <- function(yr = 2100,
                           model = c("gfdltv", "hadtv", "ipsltv", "ens"),
                           area = c("monterey_bay", "channel_islands", "fort_bragg", "san_francisco"),
                           def = "def8",
                           extents = abalone::extent_list,
                           infile = abalone::percentdays,
                           thresh = c(50, 95)
) {

  # Temporarily suppress tmap messages
  old_opt <- options(tmap.messages = FALSE)
  on.exit(options(old_opt), add = TRUE)

  tmap::tmap_options(show.messages = FALSE, component.autoscale = FALSE)

  model2 <- if (model == "^zoom") "zoom" else model

  # Load shapefiles within each worker
  eez <- read_shp(shape = "eez")
  ca <- read_shp("usa")
  base_rast <- cali_rast() %>%
    terra::project("EPSG:4326")

  r <- infile %>%
    dplyr::filter(model == model2) %>%
    dplyr::filter(year == yr)

  yr_rast <- cali_rast()
  yr_rast[terra::cells(yr_rast)] <- r$percent
  refugiaR <- yr_rast >= thresh
  refugiaRRR <- terra::app(refugiaR, fun = function(x) ifelse(x, 1, 0))
  names(refugiaRRR) <- names(yr_rast)

 #  terra::plot(refugiaRRR)

  # Crop
  if (area %in% names(extents)) {
    r <- terra::crop(refugiaRRR, terra::ext(extents[[area]]))
  }

  #terra::plot(r)

  refugiacolors <- function() {
    # Always define the palette explicitly
    custom_palette <- c("0" = "#5D3A9B",  # purple for 0
                        "1" = "#E66100")  # orange for 1
    assign("custom_palette", custom_palette, envir = globalenv())
  }

  # Map
  tt <- tmap::tm_shape(r) +
    tmap::tm_raster(col.scale = tmap::tm_scale_categorical(),
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
      col.scale = tmap::tm_scale_categorical(
        values = refugiacolors()),
      col.legend = tmap::tm_legend(title = "Refugia",
                                   position = c("right", "top"),
                                   text.size = 0.9,
                                   frame = TRUE,
                                   frame.lwd = 0.001,
                                   title.size = 1.1,
                                   bg.alpha = 0,
                                   width = 5,
                                   height = 5)
    ) +
    tmap::tm_shape(ca) +
    tmap::tm_polygons(fill_alpha = 0.1) +
    tmap::tm_title(paste0(def %>% toupper(), " | ",
                          model2, " | ", yr, " | Refugia")) +
    tmap::tm_scalebar(position = c("left", "bottom"),
                      text.size = 0.7) +
    tmap::tm_layout(
      legend.title.size = 1.4,
      bg.color = "white",
      legend.frame = FALSE)

  return(tt)
}


# yr = 2100
# model = c("gfdltv")
# area = c("monterey_bay")
# def = "def8"
# extents = abalone::extent_list
# infile = abalone::percentdays
# thresh = c(50)
