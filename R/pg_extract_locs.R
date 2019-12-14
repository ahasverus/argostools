#' @title Get Argos locations
#' @description Get Argos locations for one or several animals from a PostgreSQL database.
#'
#' @param dbname A DBI connexion information
#' @param id Animal ID
#' @param byid TRUE or FALSE
#' @param start Minimum location date
#' @param end Maximum location date
#'
#' @return A data frame with 7 variables
#'
#' @import DBI
#' @import RPostgreSQL
#'
#' @export
#'
#' @examples
#' ## See vignette
#'

pg_extract_locs <- function(dbname, id, byid = TRUE, start = NULL, end = NULL) {

	if (!is.null(start) && is.null(end))
		end <- as.character(substr(Sys.time(), 1, 10))

	if (!is.null(start) && !is.null(end) && (as.Date(start) > as.Date(end)))
		stop("Starting date has to be lower than ending date.")


	locs <- data.frame()

	for (k in 1:length(id)) {

		if (byid) {

			query <- paste("SELECT * FROM foxnoids WHERE noid = '", id[k], "';", sep = "")
			ids <- DBI::dbGetQuery(dbname, query)

			if (nrow(ids) == 0)
				stop(paste("The id #", id[k], " is absent from the database.\nIf you are looking for a ptt, please unselect byid.", sep = ""))

			for (j in 2:ncol(ids)) {

				ptt <- ids[1, j]

				if (!is.na(ptt)) {

					query <- paste("SELECT * FROM pttinfos WHERE pttuser = '", ptt, "';", sep = "")
					ptts <- DBI::dbGetQuery(dbname, query)

					if (is.null(start) && is.null(end)) {

						if (!is.na(ptts[1, "trackingend"])) {

							query <- paste("SELECT * FROM locations WHERE ",
								"platform = '", ptts[1, "pttcls"], "' AND ",
								"dateloc >= '", ptts[1, "trackingstart"], "' AND ",
								"dateloc <= '", ptts[1, "trackingend"], "';", sep = "")

						} else {

							query <- paste("SELECT * FROM locations WHERE ",
								"platform = '", ptts[1, "pttcls"], "' AND ",
								"dateloc >= '", ptts[1, "trackingstart"], "';", sep = "")
						}

					} else {

						query <- paste("SELECT * FROM locations WHERE ",
								"platform = '", ptts[1, "pttcls"], "' AND ",
								"dateloc >= '", start, "' AND ",
								"dateloc <= '", end, "';", sep = "")
					}

					res <- DBI::dbGetQuery(dbname, query)
					res <- data.frame(noid = rep(id[k], nrow(res)), res)
					locs <- rbind(locs, res)
				}
			}

		} else {

			query <- paste("SELECT * FROM pttinfos WHERE pttuser = '", id[k], "';", sep = "")
			ptts <- DBI::dbGetQuery(dbname, query)

			if (nrow(ptts) == 0)
				stop(paste("The ptt #", id[k], " is absent from the database.\nIf you are looking for a fox id, please select byid.", sep = ""))

			query <- paste(
				"SELECT noid FROM foxnoids WHERE ",
				"pttuser_1 = '", ptts[1, "pttuser"], "' OR ",
				"pttuser_2 = '", ptts[1, "pttuser"], "' OR ",
				"pttuser_3 = '", ptts[1, "pttuser"], "';",
				sep = ""
			)

			noid <- DBI::dbGetQuery(dbname, query)[1, 1]

			if (is.null(start) && is.null(end)) {

					if (!is.na(ptts[1, "trackingend"])) {

						query <- paste(
							"SELECT * FROM locations WHERE ",
							"platform = '", ptts[1, "pttcls"], "' AND ",
							"dateloc >= '", ptts[1, "trackingstart"], "' AND ",
							"dateloc <= '", ptts[1, "trackingend"], "';",
							sep = ""
						)

					} else {

						query <- paste(
							"SELECT * FROM locations WHERE ",
							"platform = '", ptts[1, "pttcls"], "' AND ",
							"dateloc >= '", ptts[1, "trackingstart"], "';",
							sep = ""
						)
					}

				} else {

					query <- paste(
						"SELECT * FROM locations WHERE ",
						"platform = '", ptts[1, "pttcls"], "' AND ",
						"dateloc >= '", start, "' AND ",
						"dateloc <= '", end, "';",
						sep = ""
					)
				}

				res <- DBI::dbGetQuery(dbname, query)
				res <- data.frame(noid = rep(noid, nrow(res)), res)
				locs <- rbind(locs, res)
		}
	}

	rownames(locs) <- NULL
	return(locs)
}
