#' Create annual stress % rasters
#'
#' This function produces a `terra:rast()` object containing annual stress rasters, defined as
#' the percentage of each year that abalone experience stress based on the definition chosen.
#'
#' @param percentdays R object. Dataframe. Defaults to `abalone::percentdays` (i.e., example package dataset of % days of stress)
#' @param esm Character. ESM model to use. Choose from `ens`, `gfdltv`, `hadtv`, `ipsltv`.
#' @param yrst Integer. Start year.
#' @param yrend Integer. End year.
#'
#' @return Produces a `terra::rast()` object of % of year stressed, for the ESM specified, for all years between `yrst` and `yrend`.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#'
#' @examples
#' ens_stress <- build_refugia_rasters(percentdays = percentdays, esm = "ens", yrst = 1990, yrend = 2100)
#' ens_stress
#' terra::plot(ens_stress[[98]], main = "% stress in 2088")
#'


build_refugia_rasters <- function(percentdays,
                                  esm = c("ens", "gfdltv", "hadtv", "ipsltv"),
                                  yrst = 1990,
                                  yrend = 2100) {

  base_rast <- cali_rast()
  results <- list()

    model_rast <- terra::rast(
      ncol = ncol(base_rast),
      nrow = nrow(base_rast),
      nlyr = length(yrst:yrend),
      extent = terra::ext(base_rast),
      crs = terra::crs(base_rast)
    )

    for (i in yrst:yrend) {

      df_sub <- subset(percentdays, model == esm) %>%
        dplyr::filter(year == i)

      yr_rast <- cali_rast()
      yr_rast[terra::cells(yr_rast)] <- df_sub$percent
      terra::time(yr_rast) <- i
      names(yr_rast) <- as.character(i)

      model_rast[[i - yrst + 1]] <- yr_rast
    }

    results[[esm]] <- model_rast
    results <- terra::rast(results)

    }




