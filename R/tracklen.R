tracklen <-
function(data, id, noid = "noid", date = "dateloc", unit = "days"){
	
	data <- data[data[ , noid] == id, ]	
	return(difftime(min(data[ , date]), max(data[ , date]), units = unit)[[1]])
}
