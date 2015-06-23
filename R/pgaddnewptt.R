pgaddnewptt <-
function(dbname, noid, ptt, start){
	
	if (missing(noid))
		stop("The fox id (noid) is required.")

	if (missing(ptt))
		stop("The ptt id (ptt) is required.")

	if (missing(start))
		stop("The activation date (start) is required.")

	# Check if it is a new fox
	query <- paste("SELECT * FROM foxnoids WHERE noid = '", noid, "';", sep = "")
	ids <- DBI::dbGetQuery(dbname, query)

	# Check if it is a new PTT
	query <- paste("SELECT * FROM pttinfos WHERE pttcls = '", ptt, "';", sep = "")
	ptts <- DBI::dbGetQuery(dbname, query)

	# Create pttuser label
	pttuser <- paste(ptt, LETTERS[nrow(ptts)+1], sep = '')
	pttuser <- gsub('A', '', pttuser)



	###
	### Insert new row in pttinfos
	###
	


	# Get fields names
	query <- paste("SELECT * FROM pttinfos LIMIT 1;", sep = "")
	labels <- colnames(DBI::dbGetQuery(dbname, query))

	# Collapse values
	query <- paste(pttuser, ptt, start, sep = "','")
	query <- paste("('", query, "'", ",Null);", sep = "")
	dbSendQuery(dbname, paste("INSERT INTO pttinfos (", paste(labels, collapse = ","), ") VALUES ", query, sep = ""))



	###
	### Create new entry if new fox
	###



	if (nrow(ids) == 0){

		# Get fields names
		query <- paste("SELECT * FROM foxnoids LIMIT 1;", sep = "")
		labels <- colnames(DBI::dbGetQuery(dbname, query))

		# Collapse values
		query <- paste(noid, pttuser, sep = "','")
		query <- paste("('", query, "'", ",Null,Null);", sep = "")
		dbSendQuery(dbname, paste("INSERT INTO foxnoids (", paste(labels, collapse = ","), ") VALUES ", query, sep = ""))
	
	

	###
	### Update pttuser field otherwise
	###



	} else {

		# Get first empty pttuser field
		field <- colnames(ids)[min(which(is.na(ids)))]

		# Send query
		dbSendQuery(dbname, paste("UPDATE foxnoids SET ", field, " = '", pttuser, "' WHERE noid = '", noid, "';", sep = ""))
	}
}
