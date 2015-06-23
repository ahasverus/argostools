pgupdatetb <-
function(data = NULL, dbname, tbname, fields){
	


	### Initial checks



	if (missing(dbname))
		stop("You have to specify the database connexion informations (dbname).")

	if (missing(tbname))
		stop("You have to specify the relation name (tbname).")

	if (missing(fields))
		stop("You have to specify the fields argument.\nSee ?pgupdatetb to learn more.")

	

	### Relation already exists and no data to add to it.



	if (length(which(dbListTables(dbname) == tbname)) == 1 &&
		is.null(data)){

		text <- "The relation already exists and no new records to insert."



	### Otherwise



	} else {

		if (!is.list(fields))
			stop("Argument fields has to be a list.\nSee ?pgupdatetb to learn more.")
		
		labels <- unlist(lapply(lapply(fields, function(x) strsplit(x, " ")), function(x) x[[1]][1]))
		names(labels) <- NULL



		### Creation of the relation structure



		if (length(which(dbListTables(dbname) == tbname)) == 0){

			query <- paste("CREATE TABLE", tbname, "(")
			
			for (i in 1 : length(fields)){
			
				if (i != length(fields)){
			
					query <- paste(query, fields[[i]], ",", sep = "")
				
				} else {
				
					query <- paste(query, fields[[i]], ");", sep = "")
				}
			}
			dbSendQuery(dbname, query)
			text <- "Table successfully created."	
		}



		### Insertion of new records



		if (!is.null(data)){
	
			for (i in 1 : nrow(data)){	

				query <- "("
		
				for (j in 1 : length(fields)){
	
					if (data[i, names(fields)[j]] == "" || is.na(data[i, names(fields)[j]])){
				
						query <- paste(query, "DEFAULT", sep = "")

					} else {
		
						query <- paste(query, "'", data[i, names(fields)[j]], "'", sep = "")
					}

					if (j != ncol(data)){

						query <- paste(query, ",", sep = "")
		
					} else {

						query <- paste(query, ");", sep = "")
					}
				}
				dbSendQuery(dbname,paste("INSERT INTO ", tbname, " (", paste(labels, collapse = ","), ") VALUES", query, sep = ""))
			}
			text <- "Table successfully updated."
		}
	}
	
	cat(text, "\n")
}
