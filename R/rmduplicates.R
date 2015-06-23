rmduplicates <-
function(data, noid = "platform", date = "dateloc"){
	
	data <- data[do.call(order, data[ , c(noid, date)]), ]
	key <- paste(data[ , noid], data[ , date], sep = "_")
	pos <- which(duplicated(key) == F)
	if (length(pos) > 0) data <- data[pos, ]
	rownames(data) <- NULL
	return(data)
}
