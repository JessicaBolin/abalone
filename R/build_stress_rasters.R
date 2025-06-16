#' Create annual stress % rasters
#'
#' This function produces a `terra:rast()` object containing annual stress rasters, defined as
#' the percentage of each year that abalone experience stress based on the definition chosen.
#'
#' @param percentdays R object. Dataframe. Defaults to `abalone::percentdays` (i.e., example package dataset of % days of stress)
#' @param esm Character. ESM model to use. Choose from `ens`, `gfdltv`, `hadtv`, `ipsltv`.
#' @param yrst Integer. Start year.
#' @param yrend Integer. End year.
#' @param progress Logical. Show progress bar? Default is TRUE.
#' @param save_path Character. Directory to save output to. Optional, set to NULL if not needed.
#'
#' @return Produces a `terra::rast()` object of % of year stressed, for the ESM specified, for all years between `yrst` and `yrend`.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#'
#' @examples
#' ens_stress <- build_stress_rasters(percentdays = abalone::percentdays, esm = "ens",
#' yrst = 1990, yrend = 1992, progress = FALSE, save_path = NULL)
#' ens_stress
#' terra::plot(ens_stress[[2]], main = "% stress in 1992")
#'
build_stress_rasters <- function(percentdays = abalone::percentdays,
                                 esm = c("ens", "gfdltv", "hadtv", "ipsltv"),
                                 yrst = 1990,
                                 yrend = 2100,
                                 progress = TRUE,
                                 save_path = NULL) {

  esm <- match.arg(esm)
  base_rast <- abalone::cali_rast()
  results <- list()

  model_rast <- terra::rast(
    ncol = ncol(base_rast),
    nrow = nrow(base_rast),
    nlyr = length(yrst:yrend),
    extent = terra::ext(base_rast),
    crs = terra::crs(base_rast)
  )

  if (progress) {
    total_years <- yrend - yrst + 1
    pb <- utils::txtProgressBar(min = 0, max = total_years, style = 3)
  }

  for (i in yrst:yrend) {

    df_sub <- subset(percentdays, model == esm) %>%
      dplyr::filter(year == i)

    yr_rast <- cali_rast()
    yr_rast[terra::cells(yr_rast)] <- df_sub$percent
    terra::time(yr_rast) <- i
    names(yr_rast) <- as.character(i)

    model_rast[[i - yrst + 1]] <- yr_rast
    if (progress) utils::setTxtProgressBar(pb, i - yrst + 1)
  }

  results[[esm]] <- model_rast
  results <- terra::rast(results)


  if (progress) close(pb)

  # Save raster to file
  if (!is.null(save_path)) {
    dir_to_save <- dirname(save_path)
    if (!dir.exists(dir_to_save)) dir.create(dir_to_save, recursive = TRUE)
    save_pathh <- paste0(save_path, "/", esm, "_stress.tif")
    terra::writeRaster(results, save_pathh, overwrite = TRUE)
    message("Raster saved to: ", normalizePath(save_pathh))
  } else if (interactive()) {
    # Developer convenience: save to inst/extdata
    dev_path <- file.path(usethis::proj_path(), "inst", "extdata",
                          paste0(esm, "_stress.tif"))
    terra::writeRaster(results, dev_path, overwrite = TRUE)
    message("Developer raster saved to: ", normalizePath(dev_path))
  }

  return(results)

}

 # For internal package development (no save path argument!)
 # build_stress_rasters(percentdays = abalone::percentdays, esm = "ens", yrst = 1990, yrend = 2100, progress = TRUE, save_path = "/Users/admin/Desktop/untitled folder")
 # build_stress_rasters(percentdays = abalone::percentdays, esm = "gfdltv", yrst = 1990, yrend = 2100, progress = TRUE)
 # build_stress_rasters(percentdays = abalone::percentdays, esm = "hadtv", yrst = 1990, yrend = 2100, progress = TRUE)
 # build_stress_rasters(percentdays = abalone::percentdays, esm = "ipsltv", yrst = 1990, yrend = 2100, progress = TRUE)
