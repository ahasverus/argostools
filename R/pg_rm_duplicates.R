#' @title Remove duplicates in PostgreSQL relation
#' @description Remove duplicates in PostgreSQL relation.
#'
#' @param dbname A DBI connexion information
#' @param tbname PostgreSQL table name in which duplicates will be removed
#'
#' @import DBI
#' @import RPostgreSQL
#'
#' @export
#'
#' @examples
#' ## See vignette
#'

pg_rm_duplicates <- function(dbname, tbname){


	### Create an empty relation from tbname structure
	dbSendQuery(
		dbname,
		paste(
			"CREATE TABLE temporary AS SELECT * FROM",
			tbname,
			"WHERE 1 = 2"
		)
	)


	### Add records without duplicates
	dbSendQuery(
		dbname,
		paste(
			"INSERT INTO temporary SELECT DISTINCT * FROM",
			tbname
		)
	)


	### Delete initial relation
	dbSendQuery(
		dbname,
		paste(
			"DROP TABLE",
			tbname
		)
	)


	### Rename new relation as tbname
	dbSendQuery(
		dbname,
		paste(
			"ALTER TABLE temporary RENAME TO",
			tbname
		)
	)
}
