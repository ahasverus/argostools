readdiagfile <-
function(file){
	
	# Check if we are in the good directory
#	if (length(which(dir() == file)) == 0)
#		stop("Please select an appropriate directory.\n")
	
	x <- readLines(file)
	
	pos <- which(x == "")
	x <- x[-pos]

	pos <- grep(" Prog ", x)
	prg <- as.numeric(strsplit(x[pos], " Prog ")[[1]][2])
	x <- x[-pos]

	# Extract PTT
	sop <- grep(" Date : ", x)
	ptt <- gsub(" ", "", unlist(lapply(strsplit(x[sop], " Date : "), function(x) x[1])))
	ptt <- as.character(as.numeric(ptt))

	# Extract Date
	lvl1 <- unlist(lapply(strsplit(x[sop], " Date : "), function(x) x[2]))
	dte <- substr(unlist(lapply(strsplit(lvl1, " LC : "), function(x) x[1])), 1, 17)
	dte <- paste("20", substr(dte, 7, 8), "/", substr(dte, 4, 5), "/", substr(dte, 1, 2), " ", substr(dte, 10, 17), sep = "")

	# Extract location class
	lvl2 <- unlist(lapply(strsplit(lvl1, " LC : "), function(x) x[2]))
	lc <- gsub(" ", "", unlist(lapply(strsplit(lvl2, " IQ : "), function(x) x[1])))
	iq <- gsub(" ", "", unlist(lapply(strsplit(lvl2, " IQ : "), function(x) x[2])))

	# Extract first coordinates
	sop <- grep(" Lat1 : ", x)
	lvl1 <- unlist(lapply(strsplit(x[sop], " Lat1 : "), function(x) x[2]))
	lat1 <- gsub("N ", "", unlist(lapply(strsplit(lvl1, " Lon1 : "), function(x) x[1])))
	lvl2 <- unlist(lapply(strsplit(lvl1, " Lon1 : "), function(x) x[2]))
	lon1 <- paste("-", gsub(" ", "", gsub("W ", "", unlist(lapply(strsplit(lvl2, " Lat2 : "), function(x) x[1])))), sep = "")

	# Extract second coordinates
	lvl3 <- unlist(lapply(strsplit(lvl2, " Lat2 : "), function(x) x[2]))
	lat2 <- gsub("N ", "", unlist(lapply(strsplit(lvl3, " Lon2 : "), function(x) x[1])))
	lon2 <- paste("-", gsub(" ", "", gsub("W", "", unlist(lapply(strsplit(lvl3, " Lon2 : "), function(x) x[2])))), sep = "")

	# Extract messages
	sop <- grep(" Nb mes : ", x)
	lvl1 <- unlist(lapply(strsplit(x[sop], " Nb mes : "), function(x) x[2]))
	mes <- as.numeric(gsub(" ", "", unlist(lapply(strsplit(lvl1, " Nb mes>-120dB : "), function(x) x[1]))))
	lvl2 <- unlist(lapply(strsplit(lvl1, " Nb mes>-120dB : "), function(x) x[2]))
	mes120 <- as.numeric(gsub(" ", "", unlist(lapply(strsplit(lvl2, " Best level : "), function(x) x[1]))))
	best <- as.numeric(gsub(" dB", "", unlist(lapply(strsplit(lvl2, " Best level : "), function(x) x[2]))))

	# Extract NOPC
	sop <- grep(" Pass duration : ", x)
	lvl1 <- unlist(lapply(strsplit(x[sop], " Pass duration : "), function(x) x[2]))
	pass <- gsub("s", "", gsub(" ", "", unlist(lapply(strsplit(lvl1, " NOPC : "), function(x) x[1]))))
	nopc <- unlist(lapply(strsplit(lvl1, " NOPC : "), function(x) x[2]))

	# Extract altitude
	sop <- grep(" Calcul freq : ", x)
	lvl1 <- unlist(lapply(strsplit(x[sop], " Calcul freq : "), function(x) x[2]))
	freq <- gsub("Hz", "", gsub(" ", "", unlist(lapply(strsplit(lvl1, " Altitude : "), function(x) x[1]))))
	alt <- gsub("m", "", gsub(" ", "", unlist(lapply(strsplit(lvl1, " Altitude : "), function(x) x[2]))))

	# Extract sensors
	#sop <- which(nchar(x) == 42)
	#sen <- gsub(" ", "", x[sop])
	#sen1 <- substr(sen, 1, 2)
	#sen2 <- substr(sen, 3, 4)
	#sen3 <- substr(sen, 5, 6)

	tab <- data.frame(Platform.ID = ptt, Platform = "", Prg.No. = prg, Latitude = lat1, Longitude = lon1, Loc.quality = lc, Loc.date = dte, Pass = pass, Altitude = alt, Frequency = freq, Long.1 = lon1, Lat.sol.1 = lat1, Long.2 = lon2, Lat.sol.2 = lat2, Loc.idx = iq, Nopc = nopc, Msg = mes, Msg.120 = mes120, Best.level = best
	#, Sensor.1 = sen1, Sensor.2 = sen2, Sensor.3 = sen3
	)

	return(tab)
}
