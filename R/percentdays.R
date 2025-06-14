#' Example dataset of % days of stress
#'
#' For refugia definition 8: growth, defined as when temperatures are between 16-19˚C.
#'
#' @format A data frame with 6 columns, and 697524 rows:
#' \describe{
#'   \item{cellID}{cellID corresponding to the cell index position of the raster of California cropped to 100m isobath}
#'   \item{model}{Earth System Model. Either "gfdltv", "hadtv", "ipsltv" or "ens"}
#'   \item{refugiadays}{Number of days classified as refugia for that year}
#'   \item{percent}{As above, but classed as a percentage value}
#'   \item{year}{Year of interest}
#' }
#'
#' @examples
#' head(percentdays)
#' str(percentdays)
"percentdays"

