pgconnectdb <-
function(driver = "PostgreSQL", host = "localhost", port = 5433, user, password, dbname){
	
	# Connexion to SQL driver
	mycon <- dbConnect(drv = driver, host = host, port = port, user = user, password = password)
	
	# List of available databases
	dbs <- dbGetQuery(mycon, "SELECT datname FROM pg_database WHERE datistemplate = false;")[,1]
	
	# Creation a the new database (if it does not exist)
	if (length(which(dbs == dbname)) == 0){
		
		dbSendQuery(mycon, paste("CREATE DATABASE ", dbname, ";", sep = ""))
		dbDisconnect(mycon)
		cat("The database ", dbname, " has been successfully created.\n")

	}
	cat(paste("R is now connected to ", dbname, ".\n", sep = ""))

	# Return of connexion informations
	return(dbConnect(drv = driver, host = host, port = port, user = user, password = password, dbname = dbname))
}
