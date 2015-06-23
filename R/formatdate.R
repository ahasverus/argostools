formatdate <-
function(data, dte = "dateloc"){
	
	pos <- which(data[ , dte] == "")
	if (length(pos) > 0) data[pos, dte] <- NA
	
	pos <- grep("/", data[ , dte])
	if (length(pos) > 0){
		yy <- strsplit(data[pos , dte], "/")
		xx <- list()
		for (i in 1 : length(yy[[1]]))
			xx[[i]] <- unlist(lapply(yy, function(x) x[i]))
			
		if (nchar(xx[[1]][1]) == 4){
			mm <- xx[[2]] < 13
			if (length(which(mm == FALSE)) == 0){
				data[pos, dte] <- as.character(as.POSIXct(strptime(as.character(data[pos, dte]),'%Y/%m/%d %H:%M:%S')))
			}else{
				data[pos, dte] <- as.character(as.POSIXct(strptime(as.character(data[pos, dte]),'%Y/%d/%m %H:%M:%S')))
			}
				
		}else{
			mm <- xx[[2]] < 13
			if (length(which(mm == FALSE)) == 0){
				data[pos, dte] <- as.character(as.POSIXct(strptime(as.character(data[pos, dte]),'%d/%m/%Y %H:%M:%S')))
			}else{
				data[pos, dte] <- as.character(as.POSIXct(strptime(as.character(data[pos, dte]),'%m/%d/%Y %H:%M:%S')))
			}
		}
	}
	
	pos <- grep("-", data[ , dte])
	if (length(pos) > 0){
		yy <- strsplit(data[pos , dte], "-")
		xx <- list()
		for (i in 1 : length(yy[[1]]))
			xx[[i]] <- unlist(lapply(yy, function(x) x[i]))
			
		if (nchar(xx[[1]][1]) == 4){
			mm <- xx[[2]] < 13
			if (length(which(mm == FALSE)) == 0){
				data[pos, dte] <- as.character(as.POSIXct(strptime(as.character(data[pos, dte]),'%Y-%m-%d %H:%M:%S')))
			}else{
				data[pos, dte] <- as.character(as.POSIXct(strptime(as.character(data[pos, dte]),'%Y-%d-%m %H:%M:%S')))
			}
				
		}else{
			mm <- xx[[2]] < 13
			if (length(which(mm == FALSE)) == 0){
				data[pos, dte] <- as.character(as.POSIXct(strptime(as.character(data[pos, dte]),'%d-%m-%Y %H:%M:%S')))
			}else{
				data[pos, dte] <- as.character(as.POSIXct(strptime(as.character(data[pos, dte]),'%m-%d-%Y %H:%M:%S')))
			}
		}
	}

	data[ , dte] <- as.POSIXct(strptime(as.character(data[ , dte]),'%Y-%m-%d %H:%M:%S'))
	return(data)
}
