stackfiles <-
function(files_list){
	
	locs <- data.frame()
	
	for (i in 1 : length(files_list)){
			
		data <- readargos(files_list[[i]])
			
		for (k in 1 : ncol(data)) data[ , k] <- as.character(data[ , k])
			
		# Extract PTTs
		ptt <- sort(unique(as.character(data[ , "platform"])))
		if (length(grep("[[:alpha:]]", ptt)) > 0)
			ptt <- ptt[-grep("[[:alpha:]]", ptt)]
		
		# Remove no PTTs rows
		pos <- which(!(data[ , "platform"] %in% ptt))
		if (length(pos) > 0)
			data <- data[-pos, ]
				
		# Format fields			
		data <- formatdate(data)
		data <- formatcoords(data)
			
		# Remove NAs
		cat(i, "\t")
		data <- deletenas(data)
			
		# Temperature convert
		#Sensor <- paste(data[ , "Sensor1"], data[ , "Sensor2"], data[ , "Sensor3"], sep = "")
		#pos <- grep("[[:punct:]]", Sensor)
		#if (length(pos) > 0) Sensor[pos] <- data[pos, "Sensor1"]
		#pos <- which(is.na(data[ , "Sensor2"]) & is.na(data[ , "Sensor3"]))
		#if (length(pos) > 0) Sensor[pos] <- data[pos, "Sensor1"]
			
		#pos <- which(is.na(data[ , "Sensor1"]) & is.na(data[ , "Sensor2"]) & is.na(data[ , "Sensor3"]))
		#if (length(pos) > 0) Sensor[pos] <- NA
			
		#Sensor <- unlist(sapply(as.character(Sensor), extractsensor, USE.NAMES = F))
		#data <- data[ , -grep("Sensor", colnames(data))]
		#data[ , "Temperature"] <- Sensor
			
		# Adding to table
		locs <- rbind(locs, data)
	}
	
	# Type conversion
	for (i in 1 : ncol(locs))
		locs[ , i] <- as.character(locs[ , i])
	
	# Remove duplicates
	locs <- rmduplicates(locs)
	
	return(locs)	
}
