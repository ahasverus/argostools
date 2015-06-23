pgupdateinfos <-
function(dbname, tbname, field, value, key, id){

	cat(paste("Warning: This operation can not be undone.\nDo you really want to update", tbname, "?\n[y/n]"))
	answer <- readLines(n = 1)

	if (answer != "y" && answer != "n")
		stop("Correct answer is 'y' or 'n'.")
	
	if (answer == "y"){

		if (value != "Null"){
			dbSendQuery(dbname, paste("UPDATE ", tbname, " SET ", field, " = '", value, "' WHERE ", key, " = '", id, "';", sep = ""))
		} else {
			dbSendQuery(dbname, paste("UPDATE ", tbname, " SET ", field, " = ", value, " WHERE ", key, " = '", id, "';", sep = ""))
		}

		cat("\nInformation successfully updated.\n")
	}
}
