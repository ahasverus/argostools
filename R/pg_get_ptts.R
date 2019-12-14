#' @title Get Animal PTT information from PostgreSQL database
#' @description Get Animal PTT information from PostgreSQL database.
#'
#' @param dbname A DBI connexion information
#' @param noid ID of the animal. If missing, all ID are processed
#'
#' @return A data frame with 4 variables
#'
#' @import DBI
#' @import RPostgreSQL
#'
#' @export
#'
#' @examples
#' ## See vignette
#'

pg_get_ptts <- function(dbname, noid = NULL){

	if (is.null(noid))
		noid <- DBI::dbGetQuery(dbname, "SELECT noid FROM foxnoids;")[ , 1]

	mat <- data.frame()
	ids <- NULL

	for (k in 1:length(noid)){

		query <- paste("SELECT * FROM foxnoids WHERE noid = '", noid[k], "';", sep = "")
		ptts <- DBI::dbGetQuery(dbname, query)

		if (nrow(ptts) == 0){

			stop(paste("The id fox #", noid[k], " is absent from the database.", sep = ""))
		}

		for (i in 2:ncol(ptts)){

			if (!is.na(ptts[1, i])){
				query <- paste("SELECT * FROM pttinfos WHERE pttuser = '", ptts[1, i], "';", sep = "")
				mat <- rbind(mat, DBI::dbGetQuery(dbname, query))
				ids <- c(ids, noid[k])
			}
		}
	}

	for (i in 1:ncol(mat)) mat[ , i] <- as.character(mat[ , i])

	return(data.frame(noid = as.character(ids), mat))
}
