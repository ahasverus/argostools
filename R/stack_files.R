#' @title Import and clean raw Argos locations data (multiple files)
#' @description Import and clean raw Argos locations data(multiple files).
#'
#' @param files Vector of filenames to read
#'
#' @return A data frame with 7 variables
#'
#' @export
#'
#' @examples
#' ## See vignette
#'


stack_files <- function(files) {

	locs <- data.frame()

	for (file in files){

		data <- read_argos(file)

		# Extract PTTs
		ptt <- sort(unique(as.character(data[ , "platform"])))

		if (length(grep("[[:alpha:]]", ptt)) > 0) {

			ptt <- ptt[-grep("[[:alpha:]]", ptt)]
		}


		# Remove no PTTs rows
		pos <- which(!(data[ , "platform"] %in% ptt))
		if (length(pos) > 0) {

			data <- data[-pos, ]
		}


		# Format fields
		data <- format_date(data)
		data <- format_coords(data)

		# Remove NAs
		data <- delete_nas(data)


		# Adding to table
		locs <- rbind(locs, data)
	}

	# Type conversion
	for (i in 1:ncol(locs)) {

		locs[ , i] <- as.character(locs[ , i])
	}


	# Remove duplicates
	locs <- rm_duplicates(locs)

	return(locs)
}
