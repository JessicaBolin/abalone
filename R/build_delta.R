#' Build rasters of persistent refugia
#'
#' This function creates a single raster of persistent refugia for an ESM of interest,
#' over a user-specified time period.
#'
#' @param esm Character. ESM model to use. Choose from `ens`, `gfdltv`, `hadtv`, `ipsltv`.
#' @param hist_range Vector of integers. Time period for historical period
#' @param proj_range Vector of integers. Time period for projection period
#' @param def Character. Refugia definition name (e.g., "def8") used in input file paths.
#' @param persist_thresh Integer. Temporal threshold (%) used for defining annual refugia. Choose either 50 (liberal) or 95% (conservative).
#' @param save_path Character. Directory to save output to. Optional, set to NULL if not needed.

#' @return Produces a `terra::rast()` object of delta refugia (%), for the ESM specified.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#'
#' @examples
#' deltar <- build_delta(persist_thresh = 50, esm = "ens",
#' hist_range = 2020:2049, proj_range = 2070:2099, save_path = NULL)
#' deltar
#' terra::plot(deltar)

build_delta <- function(persist_thresh = c(50, 95),
                        esm = c("ens", "gfdltv", "hadtv", "ipsltv"),
                        def = "def8",
                        hist_range = 2020:2049,
                        proj_range = 2070:2099,
                        save_path = NULL) {

  if (esm == "^zoom") { model2 <- "zoom" } else { model2 <- esm }

  hist <- terra::rast(system.file("extdata",
                                  paste0(model2, "_persistence_",
                                         persist_thresh, "_",
                                         min(hist_range), "-",
                                         max(hist_range), ".tif"),
                                  package = "abalone"))
  proj <- terra::rast(system.file("extdata",
                                  paste0(model2, "_persistence_",
                                         persist_thresh, "_",
                                         min(proj_range), "-",
                                         max(proj_range), ".tif"),
                                  package = "abalone"))

  delta <- proj-hist

  # Save raster to file
  if (!is.null(save_path)) {
    dir_to_save <- dirname(save_path)
    if (!dir.exists(dir_to_save)) dir.create(dir_to_save, recursive = TRUE)
    save_pathh <- paste0(save_path, "/", model2, "_delta_", persist_thresh,
                         "_", min(proj_range), "-", max(proj_range), "minus",
                         min(hist_range), "-", max(hist_range), ".tif")
    terra::writeRaster(delta, save_pathh, overwrite = TRUE)
    message("Raster saved to: ", normalizePath(save_pathh))
  } else if (interactive()) {
    # Developer convenience: save to inst/extdata
    dev_path <- file.path(usethis::proj_path(),
                          "inst", "extdata",
                          paste0(save_path, "/", model2, "_delta_",
                                 persist_thresh, "_", min(proj_range), "-",
                                 max(proj_range), "minus", min(hist_range), "-",
                                 max(hist_range), ".tif"))
    terra::writeRaster(delta, dev_path, overwrite = TRUE)
    message("Developer raster saved to: ", normalizePath(dev_path))
  }

  return(delta)

}

# for (i in c("ens", "gfdltv", "hadtv", "ipsltv")) {
#   deltarast(persist_thresh = 50, esm = i)
#   deltarast(persist_thresh = 95, esm = i)
# }

