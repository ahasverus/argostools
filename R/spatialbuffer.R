spatialbuffer <-
function(data, xlim, ylim, lat = "latitude", long = "longitude"){
	
	if (missing(xlim) || missing(ylim))
		stop("You have to specify spatial boundaries of the buffer.")
		
	pos <- which(data[ , long] >= min(xlim) & data[ , long] <= max(xlim))
	if (length(pos) > 0) data <- data[pos, ]

	pos <- which(data[ , lat] >= min(ylim) & data[ , lat] <= max(ylim))
	if (length(pos) > 0) data <- data[pos, ]

	rownames(data) <- NULL
	return(data)
}
