#' @title Get duration of animal tracking
#' @description Get duration of animal tracking.
#'
#' @param data Cleaned Argos location data frame
#' @param id The animal id to extract track informations
#' @param noid Column name of the animal id field
#' @param date Column name of the date field
#' @param unit Default is 'days'
#'
#' @return A data frame with 7 variables
#'
#' @export
#'
#' @examples
#' ## See vignette
#'

track_length <- function(data, id, noid = "noid", date = "dateloc", unit = "days"){

	if (missing(id)) {
		stop("Please provide an id to extract track informations")
	}

	if (length(id) > 1) {
		stop("Please provide one single id to extract track informations")
	}

	data <- data[data[ , noid] == id, ]

	return(difftime(min(data[ , date]), max(data[ , date]), units = unit)[[1]])
}
