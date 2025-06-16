#' Build rasters of persistent refugia
#'
#' This function creates a single raster of persistent refugia for an ESM of interest,
#' over a user-specified time period.
#'
#' @param model Character. ESM model to use. Choose from `ens`, `gfdltv`, `hadtv`, `ipsltv`.
#' @param yr_range Vector of integers. Time period.
#' @param persist_thresh Integer. Temporal threshold (%) used for defining annual refugia. Choose either 50 (liberal) or 95% (conservative).
#' @param save_path Character. Directory to save output to. Optional, set to NULL if not needed.

#' @return Produces a `terra::rast()` object of persistent refugia (%), for the ESM specified.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#'
#' @examples
#' persist_refugia <- persist_rast(model = "ens", yr_range = 2070:2099,
#' persist_thresh = 50, save_path = NULL)
#' persist_refugia
#' terra::plot(persist_refugia)


persist_rast <- function(model = c("ens", "gfdltv", "hadtv", "ipsltv"),
                    yr_range = c(1990:2019),
                    persist_thresh = c(50, 95),
                    save_path = NULL) {

  if (model == "^zoom") { model2 <- "zoom" } else { model2 <- model }

  filey <- file.path(usethis::proj_path(), "inst", "extdata",
            paste0(model2, "_refugia_", persist_thresh, ".tif"))
  biglist <- terra::rast(filey)
  sb_biglist <- biglist[[names(biglist) %in% yr_range]]
  sum_raster <- sum(sb_biglist)
 # terra::plot(sum_raster)
  persistence_raster <- sum_raster / terra::nlyr(sb_biglist) * 100
  names(persistence_raster) <- paste0(min(yr_range), "-", max(yr_range))
 # terra::plot(persistence_raster, main = "% Persistence of Refugia Over Time")

  # Save raster to file
  if (!is.null(save_path)) {
    dir_to_save <- dirname(save_path)
    if (!dir.exists(dir_to_save)) dir.create(dir_to_save, recursive = TRUE)
    save_pathh <- paste0(save_path, "/", model2, "_persistence_", persist_thresh,
                         "_", min(yr_range), "-", max(yr_range), ".tif")
    terra::writeRaster(persistence_raster, save_pathh, overwrite = TRUE)
    message("Raster saved to: ", normalizePath(save_pathh))
  } else if (interactive()) {
    # Developer convenience: save to inst/extdata
    dev_path <- file.path(usethis::proj_path(), "inst", "extdata",
                          paste0(model2, "_persistence_", persist_thresh,
                                 "_", min(yr_range), "-", max(yr_range), ".tif"))
    terra::writeRaster(persistence_raster, dev_path, overwrite = TRUE)
    message("Developer raster saved to: ", normalizePath(dev_path))
  }

  return(persistence_raster)

}

# persist(model = "ens",
#         yr_range <- 2070:2099,
#         persist_thresh = 50, save_path = NULL)
