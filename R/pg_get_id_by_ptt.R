#' @title Get Animal ID from PTT
#' @description Get Animal ID from PTT stored in a PostgreSQL database.
#'
#' @param dbname A DBI connexion information
#' @param ptt ID of one or several PTT(s)
#'
#' @return A data frame with 2 variables
#'
#' @import DBI
#' @import RPostgreSQL
#'
#' @export
#'
#' @examples
#' ## See vignette
#'

pg_get_id_by_ptt <- function(dbname, ptt) {


	mat <- data.frame()

	for (k in 1:length(ptt)) {

		query <- paste("SELECT * FROM pttinfos WHERE pttcls = '", ptt[k], "';", sep = "")
		ids <- DBI::dbGetQuery(dbname, query)

		if (nrow(ids) == 0) {

			stop(paste("The PTT #", ptt[k], " is absent from the database.", sep = ""))
		}

		lab <- NULL

		for (i in 1:nrow(ids)) {

			query <- paste("SELECT noid FROM foxnoids WHERE ",
				"pttuser_1 = '", ids[i, "pttuser"], "' OR ",
				"pttuser_2 = '", ids[i, "pttuser"], "' OR ",
				"pttuser_3 = '", ids[i, "pttuser"], "';", sep = "")
			lab[i] <- DBI::dbGetQuery(dbname, query)[1, 1]
		}
		ids <- data.frame(noid = lab, ids)
		mat <- rbind(mat, ids)
	}

	return(mat)
}
