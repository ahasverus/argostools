pgrmduplicates <-
function(dbname, tbname){
	


	### Create an empty relation from tbname structure
	x <- dbSendQuery(dbname, paste('CREATE TABLE temporary AS SELECT * FROM ', tbname, ' WHERE 1 = 2;', sep = ""))
	

	### Add records without duplicates
	x <- dbSendQuery(dbname, paste('INSERT INTO temporary SELECT DISTINCT * FROM ', tbname, ';', sep = ""))
	
	### Delete initial relation
	x <- dbSendQuery(dbname, paste('DROP TABLE ', tbname, ';', sep = ""))
	
	### Rename new relation as tbname
	x <- dbSendQuery(dbname, paste('ALTER TABLE temporary RENAME TO ', tbname, ';', sep = ""))

	rm(x)
	cat("Duplicates successfully removed.\n")
}
