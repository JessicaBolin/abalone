#' Build rasters of binary refugia
#'
#' This function creates annual rasters of refugia for an ESM of interest, using a
#' temporal threshold (where refugia conditions must be met for 50 or 95% of the year).
#' The function also displays a progress bar.
#'
#' @param percentdays R object. Dataframe. Defaults to `abalone::percentdays` (i.e., example package dataset of % days of stress)
#' @param esm Character. ESM model to use. Choose from `ens`, `gfdltv`, `hadtv`, `ipsltv`.
#' @param yrst Integer. Start year. Defaults to 1990.
#' @param yrend Integer. End year. Defaults to 2100.
#' @param thresh Integer. Temporal threshold (%) used for defining annual refugia. Choose either 50 (liberal) or 95% (conservative).
#' @param progress Logical. Show progress bar? Default is TRUE.

#' @return Produces a `terra::rast()` object of binary refugia (0/1), for the ESM specified, for all years between `yrst` and `yrend`.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#'
#' @examples
#' ens_refugia <- build_refugia_rasters(percentdays = abalone::percentdays,
#' esm = "ens", yrst = 1990, yrend = 2100, thresh = 50, progress = FALSE)
#' ens_refugia
#' terra::plot(ens_refugia[[98]], main = "Refugia in 2088")
#'
#'
build_refugia_rasters <- function(percentdays = abalone::percentdays,
                                  esm = c("ens", "gfdltv", "hadtv", "ipsltv"),
                                  yrst = 1990,
                                  yrend = 2100,
                                  thresh = c(50, 95),
                                  progress = TRUE) {

  model2 <- ifelse(esm == "^zoom", "zoom", esm)

  base_rast <- cali_rast()
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

    stress_df <- percentdays

    stress_df <- stress_df %>% dplyr::filter(model == esm) %>%
      dplyr::filter(year == i)

    yr_rast <- cali_rast()
    yr_rast[terra::cells(yr_rast)] <- stress_df$percent
    terra::time(yr_rast) <- i
    names(yr_rast) <- as.character(i)

    # Identify refugia: where stress exceeds threshold
    refugiaR <- yr_rast >= thresh
    refugiaRRR <- terra::app(refugiaR, fun = function(x) ifelse(x, 1, 0))
    names(refugiaRRR) <- names(yr_rast)

    model_rast[[i - yrst + 1]] <- refugiaRRR

    if (progress) utils::setTxtProgressBar(pb, i - yrst + 1)
  }

  if (progress) close(pb)

  return(model_rast)

}

#
# build_refugia_rasters(percentdays,
#                       esm = "ens",
#                       thresh = 50,
#                       yrst = 1990,
#                       yrend = 2100)



