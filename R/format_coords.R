#' @keywords internal

format_coords <- function(data, lat = "latitude", lon = "longitude") {

	pos <- which(data[ , lat] == "")
	if (length(pos) > 0) data[pos, lat] <- NA

	pos <- which(data[ , lon] == "")
	if (length(pos) > 0) data[pos, lon] <- NA

	data[ , lat] <- gsub(",", ".", data[ , lat])
	data[ , lon] <- gsub(",", ".", data[ , lon])

	###
	### Convert DMS format
	###

	# Northern hemisphere
	pos <- grep('"N', data[ , lat])
	if (length(pos) > 0){

		yy <- gsub(" ", "", data[pos, lat])
		dd <- mm <- ss <- rep(NA, length(yy))

		sop1 <- which(nchar(yy) == 11)
		if (length(sop1) > 0){
			dd[sop1] <- as.numeric(substr(yy[sop1], 1, 3))
			mm[sop1] <- as.numeric(substr(yy[sop1], 5, 6))
			ss[sop1] <- as.numeric(substr(yy[sop1], 8, 9))
		}

		sop1 <- which(nchar(yy) == 10)
		if (length(sop1) > 0){
			dd[sop1] <- as.numeric(substr(yy[sop1], 1, 2))
			mm[sop1] <- as.numeric(substr(yy[sop1], 4, 5))
			ss[sop1] <- as.numeric(substr(yy[sop1], 7, 8))
		}

		sop2 <- which(nchar(yy) == 9)
		if (length(sop2) > 0){
			dd[sop2] <- as.numeric(substr(yy[sop1], 1, 1))
			mm[sop2] <- as.numeric(substr(yy[sop1], 3, 4))
			ss[sop2] <- as.numeric(substr(yy[sop1], 6, 7))
		}

		data[pos, lat] <- round(dd + (mm + ss/60)/60, 5)
	}

	# Southern hemisphere
	pos <- grep('"S', data[ , lat])
	if (length(pos) > 0){

		yy <- gsub(" ", "", data[pos, lat])
		dd <- mm <- ss <- rep(NA, length(yy))

		sop1 <- which(nchar(yy) == 11)
		if (length(sop1) > 0){
			dd[sop1] <- as.numeric(substr(yy[sop1], 1, 3))
			mm[sop1] <- as.numeric(substr(yy[sop1], 5, 6))
			ss[sop1] <- as.numeric(substr(yy[sop1], 8, 9))
		}

		sop1 <- which(nchar(yy) == 10)
		if (length(sop1) > 0){
			dd[sop1] <- as.numeric(substr(yy[sop1], 1, 2))
			mm[sop1] <- as.numeric(substr(yy[sop1], 4, 5))
			ss[sop1] <- as.numeric(substr(yy[sop1], 7, 8))
		}

		sop2 <- which(nchar(yy) == 9)
		if (length(sop2) > 0){
			dd[sop2] <- as.numeric(substr(yy[sop1], 1, 1))
			mm[sop2] <- as.numeric(substr(yy[sop1], 3, 4))
			ss[sop2] <- as.numeric(substr(yy[sop1], 6, 7))
		}

		data[pos, lat] <- -1*round(dd + (mm + ss/60)/60, 5)
	}

	# Eastern hemisphere
	pos <- grep('"E', data[ , lon])
	if (length(pos) > 0){

		yy <- gsub(" ", "", data[pos, lon])
		dd <- mm <- ss <- rep(NA, length(yy))

		sop1 <- which(nchar(yy) == 11)
		if (length(sop1) > 0){
			dd[sop1] <- as.numeric(substr(yy[sop1], 1, 3))
			mm[sop1] <- as.numeric(substr(yy[sop1], 5, 6))
			ss[sop1] <- as.numeric(substr(yy[sop1], 8, 9))
		}

		sop1 <- which(nchar(yy) == 10)
		if (length(sop1) > 0){
			dd[sop1] <- as.numeric(substr(yy[sop1], 1, 2))
			mm[sop1] <- as.numeric(substr(yy[sop1], 4, 5))
			ss[sop1] <- as.numeric(substr(yy[sop1], 7, 8))
		}

		sop2 <- which(nchar(yy) == 9)
		if (length(sop2) > 0){
			dd[sop2] <- as.numeric(substr(yy[sop1], 1, 1))
			mm[sop2] <- as.numeric(substr(yy[sop1], 3, 4))
			ss[sop2] <- as.numeric(substr(yy[sop1], 6, 7))
		}

		data[pos, lon] <- round(dd + (mm + ss/60)/60, 5)
	}

	# Western hemisphere
	pos <- c(grep('"W', data[ , lon]), grep('"O', data[ , lon]))
	if (length(pos) > 0){

		yy <- gsub(" ", "", data[pos, lon])
		dd <- mm <- ss <- rep(NA, length(yy))

		sop1 <- which(nchar(yy) == 11)
		if (length(sop1) > 0){
			dd[sop1] <- as.numeric(substr(yy[sop1], 1, 3))
			mm[sop1] <- as.numeric(substr(yy[sop1], 5, 6))
			ss[sop1] <- as.numeric(substr(yy[sop1], 8, 9))
		}

		sop1 <- which(nchar(yy) == 10)
		if (length(sop1) > 0){
			dd[sop1] <- as.numeric(substr(yy[sop1], 1, 2))
			mm[sop1] <- as.numeric(substr(yy[sop1], 4, 5))
			ss[sop1] <- as.numeric(substr(yy[sop1], 7, 8))
		}

		sop2 <- which(nchar(yy) == 9)
		if (length(sop2) > 0){
			dd[sop2] <- as.numeric(substr(yy[sop1], 1, 1))
			mm[sop2] <- as.numeric(substr(yy[sop1], 3, 4))
			ss[sop2] <- as.numeric(substr(yy[sop1], 6, 7))
		}

		data[pos, lon] <- -1*round(dd + (mm + ss/60)/60, 5)
	}

	###
	### Remove cardinal informations
	###

	# Northern hemisphere
	pos <- grep('N', data[ , lat])
	if (length(pos) > 0){
		data[pos, lat] <- gsub(" ", "", unlist(strsplit(data[pos, lat], "N")))
		data[pos , lat] <- as.numeric(data[pos , lat])
	}

	# Southern hemisphere
	pos <- grep('S', data[ , lat])
	if (length(pos) > 0){
		data[pos, lat] <- gsub(" ", "", unlist(strsplit(data[pos, lat], "S")))
		data[pos , lat] <- -1*as.numeric(data[pos , lat])
	}

	# Eastern hemisphere
	pos <- grep('E', data[ , lon])
	if (length(pos) > 0){
		data[pos, lon] <- gsub(" ", "", unlist(strsplit(data[pos, lon], "E")))
		data[pos , lon] <- as.numeric(data[pos , lon])
	}

	# Western hemisphere
	zz <- NULL
	pos <- grep('W', data[ , lon])
	if (length(pos) > 0){
		data[pos, lon] <- gsub(" ", "", unlist(strsplit(data[pos, lon], "W")))
		zz <- pos
	}
	pos <- grep('O', data[ , lon])
	if (length(pos) > 0){
		data[pos, lon] <- gsub(" ", "", unlist(strsplit(data[pos, lon], "O")))
		zz <- c(zz, pos)
	}
	data[ , lon] <- as.numeric(data[ , lon])
	data[ , lat] <- as.numeric(data[ , lat])

	if (!is.null(zz)) data[zz , lon] <- -1*data[zz , lon]

	return(data)
}
