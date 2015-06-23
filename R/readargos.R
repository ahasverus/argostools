readargos <-
function(file, sep = "\t", dec = ".", header = TRUE, ...){

	if (length(grep(".xls", tolower(file))) > 0){
		
		data <- loadWorkbook(file)
		data <- readWorksheet(data, sheet = 1)

	}else{
	
		if (length(grep(".txt", tolower(file))) > 0){
		
			data <- read.delim(file, sep = sep, dec = dec, header = header, ...)

		}else{

			if (length(grep(".csv", tolower(file))) > 0){
		
				data <- read.csv(file, sep = sep, dec = dec, header = header, ...)

			}else{
				
				if (length(grep(".diag", tolower(file))) > 0){
		
					data <- readdiagfile(file)

				}else{
					
					stop("Only the following file formats are readable: '.xls', '.xlsx', '.txt', '.csv' and '.diag'.")
				}
			}
		}
	}
	
	colnames(data) <- tolower(gsub("[\\_|\\.| ]", "", colnames(data)))
	
	# Platform field
	pos <- which(colnames(data) %in% c("idno", "ndegid", "platform", "platformid", "ptt"))
	if (length(pos) == 0) stop("Fails to find platform field")
	if (length(pos) > 1){
		sop <- grep("id", colnames(data)[pos])
		if (length(sop) == 0) sop <- 1
		pos <- pos[sop]
	}
	tab <- data.frame(platform = as.character(data[ , pos]))
	
	# Latitude field
	pos <- which(colnames(data) == "latitude")
	if (length(pos) != 1) stop("Fails to find latitude field")
	tab <- data.frame(tab, latitude = as.character(data[ , pos]))
	
	# Longitude field
	pos <- which(colnames(data) == "longitude")
	if (length(pos) != 1) stop("Fails to find longitude field")
	tab <- data.frame(tab, longitude = as.character(data[ , pos]))
	
	# Date field
	pos <- grep("date", colnames(data))
	sop <- grep("loc", colnames(data)[pos])
	if (length(sop) != 1) stop("Fails to find location date field")
	tab <- data.frame(tab, dateloc = as.character(data[ , pos[sop]]))
	
	# Quality field
	pos <- grep("qualit", colnames(data))
	if (length(pos) == 0) pos <- grep("class", colnames(data))
	if (length(pos) != 1) stop("Fails to find location quality field")
	tab <- data.frame(tab, quality = as.character(data[ , pos]))
	
	# Sensor 1 field
	#pos <- which(colnames(data) %in% c("temperature", "sensor1", "sensor01", "x1"))
	#if (length(pos) == 1){
#		tab <- data.frame(tab, Sensor1 = as.character(data[ , pos]))
#	}else{
#		tab <- data.frame(tab, Sensor1 = NA)
#	}

	# Sensor 2 field
#	pos <- which(colnames(data) %in% c("sensor2", "sensor02", "x2"))
#	if (length(pos) == 1){
#		tab <- data.frame(tab, Sensor2 = as.character(data[ , pos]))
#	}else{
#		tab <- data.frame(tab, Sensor2 = NA)
#	}
	
	# Sensor 3 field
#	pos <- which(colnames(data) %in% c("sensor3", "sensor03", "x3"))
#	if (length(pos) == 1){
#		tab <- data.frame(tab, Sensor3 = as.character(data[ , pos]))
#	}else{
#		tab <- data.frame(tab, Sensor3 = NA)
#	}

	# Semi major axis field
	pos <- grep("grandaxe", colnames(data))
	if (length(pos) == 0) pos <- grep("majoraxis", colnames(data))
	if (length(pos) == 1){
		tab <- data.frame(tab, semmajaxis = as.character(data[ , pos]))
	}else{
		tab <- data.frame(tab, semmajaxis = NA)
	}

	# GDOP field
	pos <- which(colnames(data) == "gdop")
	if (length(pos) == 1){
		tab <- data.frame(tab, gdop = as.character(data[ , pos]))
	}else{
		tab <- data.frame(tab, gdop = NA)
	}
	
	return(tab)
}
