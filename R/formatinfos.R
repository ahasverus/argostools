formatinfos <-
function(data, noid = 'noid', ptt = 'platform', start = 'start', end = 'end'){
	
	ids <- sort(as.character(unique(data[ , noid])))
	maxptt <- max(tapply(data[ , noid], data[ , noid], length))

	dataFox <- as.data.frame(matrix(ncol = 1+maxptt, nrow = length(ids)))
	colnames(dataFox) <- c("noid", paste("pttuser", 1 : maxptt, sep = "_"))

	for (i in 1 : length(ids)){
	
		dataFox[i, "noid"] <- ids[i]
		pos <- which(data[ , noid] == ids[i])
	
		for (j in 1 : length(pos))
			dataFox[i, j+1] <- as.character(data[pos[j], ptt])
	}

	for (i in 2 : ncol(dataFox)){
		pos <- which(is.na(dataFox[ , i]))
		if (length(pos) > 0) dataFox[pos, i] <- ""
	}
	
	dataPTT <- data.frame()
	for (i in 1 : nrow(dataFox)){	
		for (j in 2 : ncol(dataFox)){		
			
			if (dataFox[i, j] != ""){			
			
				pos <- which(data[ , noid] == dataFox[i, "noid"] & data[ , ptt] == dataFox[i, j])
				sta <- substr(as.character(data[pos, start]), 1, 10)

				if (data[pos, end] == "PTT actif" || data[pos, end] == "" || is.na(data[pos, end])){
					stop <- ""
				}else{
					stop <- substr(as.character(data[pos, end]), 1, 10)
				}
				cls <- gsub("[[:alpha:]]","", dataFox[i, j])
				x <- data.frame(pttuser = dataFox[i, j], pttcls = cls, start = sta, end = stop)
				dataPTT <- rbind(dataPTT, x)
			}
		}
	}
	for (i in 1 : ncol(dataPTT)) dataPTT[ , i] <- as.character(dataPTT[ , i])
	dataPTT <- dataPTT[order(as.numeric(dataPTT[ , "pttcls"])), ]
	rownames(dataPTT) <- NULL
	
	return(list(tabIds = dataFox, tabPtt = dataPTT))
}
