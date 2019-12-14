#' @keywords internal

delete_nas <- function(data, lat = "latitude", lon = "longitude", dte = "dateloc", ptt = "platform") {

	x <- nrow(data)

	pos <- c(which(is.na(data[ , lat])), which(data[ , lat] == ""))
	if (length(pos) > 0) data <- data[-pos, ]

	pos <- c(which(is.na(data[ , lon])), which(data[ , lon] == ""))
	if (length(pos) > 0) data <- data[-pos, ]

	pos <- c(which(is.na(as.character(data[, dte]))), which(as.character(data[, dte]) == ""))
	if (length(pos) > 0) data <- data[-pos, ]

	pos <- c(which(is.na(data[ , ptt])), grep("[[:alpha:]]", data[ , ptt]), which(data[ , ptt] == ""))
	if (length(pos) > 0) data <- data[-pos, ]

	rownames(data) <- NULL

	cat(x - nrow(data), " rows were deleted.\n")

	return(data)
}
