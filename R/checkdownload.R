checkdownload <-
function(file, dest){
	
	path <- getwd()
	
	data <- readargos(file)
	
	for (i in 1 : ncol(data)) data[ , i] <- as.character(data[ , i])

	# Extract PTTs
	ptt <- sort(unique(as.character(data[ , "platform"])))
	if (length(grep("[[:alpha:]]", ptt)) > 0)
		ptt <- ptt[-grep("[[:alpha:]]", ptt)]
		
	# Remove no PTTs rows
	pos <- which(!(data[ , "platform"] %in% ptt))
	if (length(pos) > 0)
		data <- data[-pos, ]
		
	# Synthesis table
	res <- as.data.frame(matrix(nrow = length(ptt), ncol = 3))
	colnames(res) <- c("PTT", "Locations", "Area")
	res[ , "PTT"] <- ptt
	
	# Remove NAs coordinates
	data[ , "longitude"] <- as.numeric(data[ , "longitude"])
	data[ , "latitude"] <- as.numeric(data[ , "latitude"])
	pos <- which(is.na(data[ , "longitude"]) | is.na(data[ , "latitude"]))
	if (length(pos) > 0)
		data <- data[-pos, ]
		
	# Attribute table
	tab <- data.frame(Platform = data[ , "platform"], Date = data[ , "dateloc"])
		
	# Conversion into spatial points
	data <- SpatialPointsDataFrame(coords = data[ , c("longitude", "latitude")], data = tab)
	data@proj4string <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

	# Subdirectory creation
	dir.create(paste(dest, "/map_", strsplit(strsplit(file, "ArgosData_")[[1]][2], ".xls")[[1]], sep = ""), showWarnings = F)
	setwd(paste(dest, "/map_", strsplit(strsplit(file, "ArgosData_")[[1]][2], ".xls")[[1]], sep = ""))
	
	k <- 0
	
	# Species diagnostic
	for (i in 1 : nrow(res)){
		
		# Species locations subset
		temp <- data[which(data@data[ , "Platform"] == res[i, "PTT"]), ]
		
		# Number of locations
		res[i, "Locations"] <- nrow(temp@data)
		
		if (nrow(temp) > 0){
			
			# MCP area
			proj <- spTransform(temp, CRS("+proj=utm +zone=17 +datum=NAD83"))
			if (length(proj) > 4)
				res[i, "Area"] <- round(mcp.area(proj, unin = "m", unout = "km2", percent = 90, plotit = F)$a)

			# Mapping			
			temp <- SpatialPoints(Mercator(temp))
		
			tiff(paste(getwd(), "/", res[i, "PTT"], ".tif", sep = ""), width = 960, height = 960)
			par(mar = c(0, 0, 0, 0))
			plot(gmap('Bylot island', type = "satellite"))
			plot(temp, add = T, pch = "o", col = "red", cex = 2)
			legend("top", res[i, "PTT"], bty = "n", cex = 3)
			dev.off()

		}else{
			k <- k + 1
			if (k == 1){
				cat("The following ptts have no location:\n")
				cat(res[i, "PTT"], "\n")
			}else{
				cat(res[i, "PTT"], "\n")
			}
		}
	}
	
	#tiff(paste(getwd(), "/Area_Size_mcp95.tif", sep = ""), width = 1000, height = 750)
	#barplot(res[order(res[,3], na.last = NA), 3], names.arg = res[order(res[,3], na.last = NA), 1], horiz = T, col = "black", las = 1, cex.names = 0.75)
	#title("MCP area", cex = 1)
	#dev.off()

	write.table(res, paste(getwd(), "/log_file.txt", sep = ""), col.names = T, row.names = F, sep = "\t")
	setwd(path)
	return(res)
}
