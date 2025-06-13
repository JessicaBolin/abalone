#' Plot percent of days meeting refugia conditions over time
#'
#' This function produces a `ggplot2` object of an annual time series of refugia
#' across all ESMs, including the ensemble mean. The plot includes dashed horizontal
#' lines for the two temporal thresholds used to define refugia: 95 and 50%. Note
#' this function will only work for refugia definitions that are calculated using
#' % of year defined as refugia (i.e., not Definitions 1-2).
#'
#' @param area Character. Name of the area; must match a key in `abalone::extent_list`.
#' @param yr_range Numeric vector. Range of years to include. Default is 1990–2100.
#' @param def Character. Refugia definition name (e.g., "def5") used in input file paths.
#' @param input_dir Character. Parent directory where input files live. Defaults to `/out`
#' @param cons_thresh Integer. Value representing the conservative temporal threshold to define refugia. Defaults to 95(%)
#' @param lib_thresh Integer. Value representing the liberal temporal threshold to define refugia. Defaults to 50(%)
#'
#' @return Produces a `ggplot2::ggplot` object of an annual time series of refugia.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual labs theme_minimal
#' @importFrom ggplot2 ylim theme element_blank element_text unit geom_hline
#'
#' @examples
#' percentdays_ts(area = "monterey_bay", yr_range = 1990:2100, def = "def5", input_dir = "/Users/admin/Documents/GitHub/savingabalone/out", cons_thresh = 95, lib_thresh = 50)
#'

percentdays_ts <- function(area = c("monterey_bay", "channel_islands", "fort_bragg", "san_francisco"),
                           yr_range = 1990:2100,
                           def = c("def3", "def4", "def5", "def6", "def7", "def8"),
                           input_dir = "/Users/admin/Documents/GitHub/savingabalone/out",
                           cons_thresh = 95,
                           lib_thresh = 50) {

  # Validate area
  if (!area %in% names(extent_list)) {
    stop("Area not found in extent_list.")
  }

  # Get bounding box
  ex_vals <- extent_list[[area]]
  ex <- terra::ext(ex_vals["xmin"], ex_vals["xmax"], ex_vals["ymin"], ex_vals["ymax"])

  # Construct file paths
  gfdltv_path <- file.path(input_dir, def, "_2_percent_days_rasts", "gfdltv_percentdays.nc")
  hadtv_path  <- file.path(input_dir, def, "_2_percent_days_rasts", "hadtv_percentdays.nc")
  ipsltv_path <- file.path(input_dir, def, "_2_percent_days_rasts", "ipsltv_percentdays.nc")

  # Load and crop rasters
  tt_gfdl <- terra::rast(gfdltv_path) %>% terra::crop(ex)
  tt_ipsl <- terra::rast(ipsltv_path) %>% terra::crop(ex)
  tt_had  <- terra::rast(hadtv_path)  %>% terra::crop(ex)

  emplist <- vector("list", length = length(yr_range))

  for (i in 1:length(yr_range)) {

    rasty <- tt_gfdl[[i]]
    total <- values(rasty, na.rm=T) %>% sum # Sum all the cells in this area
    total <- total/values(rasty, na.rm = T) %>% length
    df_gfdl <- data.frame(model = "gfdltv", val = total, year = yr_range[i])

    rasty <- tt_ipsl[[i]]
    total <- values(rasty, na.rm=T) %>% sum
    total <- total/values(rasty, na.rm = T) %>% length
    df_ipsl <- data.frame(model = "ipsltv", val = total, year = yr_range[i])

    rasty <- tt_had[[i]]
    total <- values(rasty, na.rm=T) %>% sum
    total <- total/values(rasty, na.rm = T) %>% length
    df_had <- data.frame(model = "hadtv", val = total, year = yr_range[i])

    df <- rbind(df_gfdl, df_ipsl, df_had)

    ensdf <- data.frame(model = "ens",
                        year = yr_range[i],
                        val = (df_gfdl$val + df_had$val +df_ipsl$val)/3)
    df <- rbind(df, ensdf)
    emplist[[i]] <- df

  }

  all1 <- do.call(rbind, emplist)

  # Plot
  ggplot2::ggplot(data = all1, ggplot2::aes(x = year)) +
    ggplot2::geom_line(data = dplyr::filter(all1, model == "gfdltv"),
                       ggplot2::aes(y = val, color = "gfdltv"), size = 0.4) +
    ggplot2::geom_line(data = dplyr::filter(all1, model == "hadtv"),
                       ggplot2::aes(y = val, color = "hadtv"), size = 0.4) +
    ggplot2::geom_line(data = dplyr::filter(all1, model == "ipsltv"),
                       ggplot2::aes(y = val, color = "ipsltv"), size = 0.4) +
    ggplot2::geom_line(data = dplyr::filter(all1, model == "ens"),
                       ggplot2::aes(y = val, color = "ens"), size = 1.5) +
    ggplot2::scale_color_manual(values = c(
      "ens" = "black",
      "gfdltv" = "#8E4A49",
      "ipsltv" = "#BDC4A7",
      "hadtv" = "#3F7CAC"
    ), name = "Model") +
    ggplot2::labs(
      title = paste0(def, " | ", area, " | % of year refugia conditions met"),
      x = "Year", y = "% Days"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::ylim(0, 100) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(c(1, 0.1, 0.1, 0.1), "cm"),
      axis.title = ggplot2::element_text(size = 16, family = "Arial Narrow", face = "bold"),
      axis.text = ggplot2::element_text(size = 14, family = "Arial Narrow"),
      legend.title = ggplot2::element_text(size = 14, family = "Arial Narrow"),
      legend.text = ggplot2::element_text(size = 12, family = "Arial Narrow"),
      title = ggplot2::element_text(size = 14, family = "Arial Narrow")
    ) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = lib_thresh), lty = "dashed") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = cons_thresh), lty = "dashed")

}
