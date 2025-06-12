#' Read in shapefiles provided by this package
#'
#'This function uses the `sf` package to read one of the included shapefiles bundled with the package. Jessie has standardized these.
#'
#' @param shape One of the default shapefiles to use. Options are:
#'   \itemize{
#'     \item \code{"usa"} – The contiguous United States land boundary.
#'     \item \code{"eez"} – The U.S. West Coast EEZ
#'     \item \code{"contshelf50"} – The 50 m isobath
#'     \item \code{"contshelf200"} – The 200m isobath (i.e., the continental shelf)
#'     \item \code{"mpas"} – Californian marine protected areas
#'   }
#'
#' @return An `sf` object of the chosen shapefile
#'
#' @source
#' The shapefiles are stored in \code{/extdata}.
#'
#' \strong{Available shapefiles:}
#' \itemize{
#'   \item \strong{usa}: \code{usa_contiguous.shp}
#'   \item \strong{eez}: \code{westcoast_eez.shp}. Derived from 'Marine Regions' dataset: https://www.marineregions.org/
#'   \item \strong{contshelf50}: \code{cont_shelf_50m.shp}. Derived from GEBCO 2024
#'   \item \strong{contshelf200}: \code{cont_shelf_200m.shp}. Derived from GEBCO 2024.
#'   \item \strong{mpas}: \code{california_mpas.shp}. From CDFW dataset: 'California Marine Protected Areas ds582', representing all MPAs as of 1 Jan, 2019.

#' }
#' @export

read_shp <- function(shape = c("usa", "eez", "contshelf50", "contshelf200", "mpas")) {

  # Ensure sf is available
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required but not installed.")
  }

  # Match the argument
  shape <- base::match.arg(shape) #ensures user input is one of these and throws error if its not

  # Choose the correct file based on input, switch() is like ifelse() but so much better
  shp_file <- switch(
    shape,
    usa = base::system.file("extdata", "usa_contiguous.shp", package = "abalone"),
    eez = base::system.file("extdata", "westcoast_eez.shp", package = "abalone"),
    contshelf50 = base::system.file("extdata", "cont_shelf_50m.shp", package = "abalone"),
    contshelf200 = base::system.file("extdata", "cont_shelf_200m.shp", package = "abalone"),
    mpas = base::system.file("extdata", "california_mpas.shp", package = "abalone")

  )

  # Check if file was found
  if (shp_file == "") {
    stop(base::paste("Shapefile for", shape, "not found in extdata."))
  }

  # Read the shapefile using sf
  sf::st_read(shp_file, quiet = TRUE)
}
