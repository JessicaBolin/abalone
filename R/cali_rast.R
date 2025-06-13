#' ROMS 3km California subsetted to 100m depth
#'
#' This is a `terra::rast` object of the California coast, subsetted to only include cells above 100 m depth.
#' @param pth Internal package file path to where the `.tif` is stored
#' @source Custom defined by project team
#' @examples
#' emptyrast_100 <- cali_rast()
#' emptyrast_100
#' terra::plot(emptyrast_100)
#'
#' @export
cali_rast <- function(pth = system.file("extdata", "emptyrast_100.tif", package = "abalone")) {
  terra::rast(pth)
}

