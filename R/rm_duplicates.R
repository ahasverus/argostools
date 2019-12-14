#' @keywords internal

rm_duplicates <- function(data, noid = "platform", date = "dateloc") {

	data <- data[do.call(order, data[ , c(noid, date)]), ]
	key <- paste(data[ , noid], data[ , date], sep = "_")
	data <- data[which(!duplicated(key)), ]
	rownames(data) <- NULL

	return(data)
}
