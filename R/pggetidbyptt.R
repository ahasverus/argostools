pggetidbyptt <-
function(dbname, ptt){


	mat <- data.frame()

	for (k in 1 : length(ptt)){
	
		query <- paste("SELECT * FROM pttinfos WHERE pttcls = '", ptt[k], "';", sep = "")
		ids <- DBI::dbGetQuery(dbname, query)

		if (nrow(ids) == 0){

			stop(paste("The PTT #", ptt[k], " is absent from the database.", sep = ""))
		}

		lab <- NULL

		for (i in 1 : nrow(ids)){

			query <- paste("SELECT noid FROM foxnoids WHERE ", 
				"pttuser_1 = '", ids[i, "pttuser"], "' OR ", 
				"pttuser_2 = '", ids[i, "pttuser"], "' OR ", 
				"pttuser_3 = '", ids[i, "pttuser"], "';", sep = "")
			lab[i] <- DBI::dbGetQuery(dbname, query)[1, 1]
		}
		ids <- data.frame(noid = lab, ids)
		mat <- rbind(mat, ids)
		#for (j in 1 : ncol(mat)) mat[ , j] <- as.character(mat[ , j])
	}
	return(mat)
}
