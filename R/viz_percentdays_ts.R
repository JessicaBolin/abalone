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
#' @param def Character. Refugia definition name (e.g., "def8") used in input file paths.
#' @param input_file R Object. Dataframe of % of year stressed. Defaults to `abalone::percentdays`
#' @param cons_thresh Integer. Value representing the conservative temporal threshold to define refugia. Defaults to 95(%)
#' @param lib_thresh Integer. Value representing the liberal temporal threshold to define refugia. Defaults to 50(%)
#' @param extent_list List of vectors. Defaults to `abalone::extent_list`

#' @return Produces a `ggplot2::ggplot` object of an annual time series of refugia.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual labs theme_minimal
#' @importFrom ggplot2 ylim theme element_blank element_text unit geom_hline
#'
#' @examples
#' viz_percentdays_ts(area = "monterey_bay", yr_range = 1990:2100, def = "def8",
#' input_file = abalone::percentdays, cons_thresh = 95, lib_thresh = 50)

viz_percentdays_ts <- function(area = c("monterey_bay", "channel_islands", "fort_bragg", "san_francisco"),
                           yr_range = 1990:2100,
                           def = "def8",
                           input_file = abalone::percentdays,
                           cons_thresh = 95,
                           lib_thresh = 50,
                           extent_list = abalone::extent_list) {

  #utils::globalVariables(c("year", "model", "val"))

  # Validate area
  if (!area %in% names(extent_list)) {
    stop("Area not found in extent_list.")
  }

  # Get bounding box
  ex_vals <- extent_list[[area]]
  ex <- terra::ext(ex_vals["xmin"], ex_vals["xmax"], ex_vals["ymin"], ex_vals["ymax"])

  # Create base raster
  base_rast <- cali_rast()
  n_layers <- length(yr_range)

  # Preallocate raster stacks for each model
  gfdl_stack <- terra::rast(ncol = ncol(base_rast),
                            nrow = nrow(base_rast),
                            nlyr = n_layers,
                            extent = terra::ext(base_rast),
                            crs = terra::crs(base_rast))  %>% terra::crop(ex)
  ipsl_stack <- terra::rast(gfdl_stack) %>% terra::crop(ex)
  hadtv_stack <- terra::rast(gfdl_stack) %>% terra::crop(ex)

  for (i in seq_along(yr_range)) {

    yr <- yr_range[i]

    all_cells <- terra::cells(base_rast)
    cell_coords <- terra::xyFromCell(base_rast, all_cells)
    cell_df <- data.frame(cellID = all_cells, x = cell_coords[,1], y = cell_coords[,2])
    cell_df_cropped <- dplyr::filter(cell_df,
                                     x >= ex[1] & x <= ex[2] & y >= ex[3] & y <= ex[4])

    # GFDL
    gfdl_df <- input_file %>% dplyr::filter(model == "gfdltv", year == yr)
    gfdl_df_cropped <- dplyr::filter(gfdl_df, cellID %in% cell_df_cropped$cellID)
    gfdl_rast <- base_rast %>% terra::crop(ex)
    gfdl_rast[terra::cells(gfdl_rast)] <- gfdl_df_cropped$percent
    gfdl_stack[[i]] <- gfdl_rast
    names(gfdl_stack)[i] <- as.character(yr)

    # IPSL
    ipsl_df <- input_file %>% dplyr::filter(model == "ipsltv", year == yr)
    ipsl_df_cropped <- dplyr::filter(ipsl_df, cellID %in% cell_df_cropped$cellID)
    ipsl_rast <- base_rast %>% terra::crop(ex)
    ipsl_rast[terra::cells(ipsl_rast)] <- ipsl_df_cropped$percent
    ipsl_stack[[i]] <- ipsl_rast
    names(ipsl_stack)[i] <- as.character(yr)

    # IPSL
    had_df <- input_file %>% dplyr::filter(model == "hadtv", year == yr)
    had_df_cropped <- dplyr::filter(had_df, cellID %in% cell_df_cropped$cellID)
    had_rast <- base_rast %>% terra::crop(ex)
    had_rast[terra::cells(had_rast)] <- had_df_cropped$percent
    hadtv_stack[[i]] <- had_rast
    names(hadtv_stack)[i] <- as.character(yr)
  }

  emplist <- vector("list", length = length(yr_range))

  for (i in 1:length(yr_range)) {

    rasty <- gfdl_stack[[i]]
    total <- terra::values(rasty, na.rm=T) %>% sum # Sum all the cells in this area
    total <- total/terra::values(rasty, na.rm = T) %>% length
    df_gfdl <- data.frame(model = "gfdltv", val = total, year = yr_range[i])

    rasty <- ipsl_stack[[i]]
    total <- terra::values(rasty, na.rm=T) %>% sum
    total <- total/terra::values(rasty, na.rm = T) %>% length
    df_ipsl <- data.frame(model = "ipsltv", val = total, year = yr_range[i])

    rasty <- hadtv_stack[[i]]
    total <- terra::values(rasty, na.rm=T) %>% sum
    total <- total/terra::values(rasty, na.rm = T) %>% length
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
      axis.title = ggplot2::element_text(size = 16, face = "bold"),
      axis.text = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 12),
      title = ggplot2::element_text(size = 14)
    ) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = lib_thresh), lty = "dashed") +
    ggplot2::geom_hline(ggplot2::aes(yintercept = cons_thresh), lty = "dashed")

}
