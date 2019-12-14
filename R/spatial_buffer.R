#' @title Remove Argos location outside a spatial buffer
#' @description Remove Argos location outside a spatial buffer set by user.
#'
#' @param data Cleaned Argos location data frame
#' @param xlim Min and Max of longitude of the spatial buffer
#' @param ylim Min and Max of latitude of the spatial buffer
#' @param lat Column name of the latitude field
#' @param long Column name of the longitude field
#'
#' @return A data frame with 7 variables
#'
#' @export
#'
#' @examples
#' ## See vignette
#'

spatial_buffer <- function(data, xlim, ylim, lat = "latitude", long = "longitude") {

	if (missing(xlim) || missing(ylim))
		stop("You have to specify spatial boundaries of the buffer.")

	pos <- which(data[ , long] >= min(xlim) & data[ , long] <= max(xlim))
	if (length(pos) > 0) data <- data[pos, ]

	pos <- which(data[ , lat] >= min(ylim) & data[ , lat] <= max(ylim))
	if (length(pos) > 0) data <- data[pos, ]

	rownames(data) <- NULL

	return(data)
}
