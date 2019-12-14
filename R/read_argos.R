#' @title Import and clean raw Argos locations data
#' @description Import and clean raw Argos locations data.
#'
#' @param file Path to the filename to read
#'
#' @return A data frame with 7 variables
#'
#' @importFrom readxl read_excel
#' @importFrom readr read_delim read_csv
#'
#' @export
#'
#' @examples
#' ## See vignette
#'

read_argos <- function(file) {

	if (length(grep("\\.xls|\\.xlsx|\\.txt|\\.csv|\\.diag", tolower(file))) > 0) {

		stop("Allowed formats: '.xls', '.xlsx', '.txt', '.csv' and '.diag'")
	}

	if (length(grep("\\.xls|\\.xlsx", tolower(file))) > 0) {

		data <- readxl::read_excel(path = file, sheet = 1)
	}

	if (length(grep("\\.txt", tolower(file))) > 0) {

		data <- readr::read_delim(file)
	}

	if (length(grep("\\.csv", tolower(file))) > 0) {

		data <- readr::read_csv(file)
	}

	if (length(grep("\\.diag", tolower(file))) > 0) {

		data <- read_diag(file)
	}


	data <- as.data.frame(data)
	colnames(data) <- tolower(gsub("[[:punct:]]|[[:space:]]", "", colnames(data)))



	## Platform field

	labels <- c("idno", "ndegid", "platform", "platformid", "ptt")
	pos <- which(colnames(data) %in% labels)

	if (length(pos) == 0) {

		stop("Fails to find <platform> field")
	}

	if (length(pos) > 1) {

		sop <- grep("id", colnames(data)[pos])

		if (length(sop) == 0) {

			sop <- 1
		}

		pos <- pos[sop]
	}

	tab <- data.frame(platform = as.character(data[ , pos]), stringsAsFactors = FALSE)



	## Latitude field

	pos <- which(colnames(data) == "latitude")
	if (length(pos) != 1) {

		stop("Fails to find latitude field")
	}

	tab <- data.frame(tab, latitude = as.character(data[ , pos]), stringsAsFactors = FALSE)




	## Longitude field

	pos <- which(colnames(data) == "longitude")

	if (length(pos) != 1) {

		stop("Fails to find longitude field")
	}

	tab <- data.frame(tab, longitude = as.character(data[ , pos]), stringsAsFactors = FALSE)




	## Date field

	pos <- grep("date", colnames(data))
	sop <- grep("loc", colnames(data)[pos])

	if (length(sop) != 1) {

		stop("Fails to find location date field")
	}

	tab <- data.frame(tab, dateloc = as.character(data[ , pos[sop]]), stringsAsFactors = FALSE)




	## Quality field

	pos <- grep("qualit", colnames(data))

	if (length(pos) == 0) {

		pos <- grep("class", colnames(data))
	}

	if (length(pos) != 1) {

		stop("Fails to find location quality field")
	}

	tab <- data.frame(tab, quality = as.character(data[ , pos]), stringsAsFactors = FALSE)




	## Semi major axis field

	pos <- grep("grandaxe", colnames(data))
	if (length(pos) == 0) {

		pos <- grep("majoraxis", colnames(data))
	}

	if (length(pos) == 1) {

		tab <- data.frame(tab, semmajaxis = as.character(data[ , pos]))

	} else {

		tab <- data.frame(tab, semmajaxis = NA, stringsAsFactors = FALSE)
	}




	## GDOP field

	pos <- which(colnames(data) == "gdop")

	if (length(pos) == 1) {

		tab <- data.frame(tab, gdop = as.character(data[ , pos]))

	} else {

		tab <- data.frame(tab, gdop = NA, stringsAsFactors = FALSE)
	}

	return(tab)
}
