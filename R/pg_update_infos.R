#' @title Update values on a PostgreSQL table
#' @description Update values on a PostgreSQL table.
#'
#' @param dbname A DBI connexion information
#' @param tbname PostgreSQL table name to be updated
#' @param field Field to be updated
#' @param value New value of the field
#' @param key Key of the condition
#' @param id Value  of the condition
#'
#' @import DBI
#' @import RPostgreSQL
#'
#' @export
#'
#' @examples
#' ## See vignette
#'

pg_update_infos <- function(dbname, tbname, field, value, key, id){

	cat(paste0("Warning: This operation can not be undone.\nDo you really want to update ", tbname, "? [y/n]"))

	answer <- readLines(n = 1)

	if (tolower(answer) != "y" && tolower(answer) != "n") {

		stop("Correct answer is 'y' or 'n'.")
	}

	if (tolower(answer) == "y"){

		if (value != "null"){

			dbSendQuery(
				dbname,
				paste0(
					"UPDATE ", tbname,
					" SET ", field, " = '", value, "'",
					" WHERE ", key, " = '", id, "'"
				)
			)

		} else {

			dbSendQuery(
				dbname,
				paste0(
					"UPDATE ", tbname,
					" SET ", field, " = ", value,
					" WHERE ", key, " = '", id, "'"
				)
			)
		}

		cat("\nInformation successfully updated.\n")
	}
}
