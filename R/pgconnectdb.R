pgconnectdb <- 
function (driver = "PostgreSQL", host = "localhost", port = 5432, user, password, dbname){
    
    system('psql -lo ./tmp.txt')
    x <- unlist(lapply(strsplit(readLines('./tmp.txt'), '\\|'), 
        function(x) x[1]))[-c(1:3)]

    if (length(grep('\\(', x)) > 0) x <- x[-grep('\\(', x)]
    x <- x[which(!is.na(x))]
    x <- gsub('[[:space:]]+', '', x)
    x <- x[which(x != '')]
    
    system('rm ./tmp.txt')

    if (length(which(x == dbname)) == 0){
        system(paste('createdb', dbname))
        cat("The database", dbname, "has been successfully created.\n")
    } else {
    	cat("The database", dbname, "already exists.\n")
    }

    cat(paste("R is now connected to ", dbname, ".\n", sep = ""))
    return(dbConnect(drv = driver, host = host, port = port, user = user, password = password, dbname = dbname))
}
