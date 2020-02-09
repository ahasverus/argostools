#' @title Get Argos locations
#' @description Get Argos locations for one or several animals from a PostgreSQL database.
#'
#' @param dbname A DBI connexion information
#' @param id Animal ID (not PTT)
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

pg_extract_locs <- function(dbname, id = NULL, start = NULL, end = NULL) {

	if (!is.null(start) && is.null(end)) {

		end <- as.character(substr(Sys.time(), 1, 10))
	}

	if (is.null(start) && !is.null(end)) {

		start <- "1900-01-01"
	}

	if (!is.null(start) && !is.null(end) && (as.Date(start) > as.Date(end))) {

		stop("Starting date has to be lower than ending date.")
	}

	if (is.null(id)) {

		query <- paste0(
			"SELECT",
			" code_couleur ",
			"FROM",
			" argos_id_colliers "
		)
		id   <- DBI::dbGetQuery(dbname, query)[ , "code_couleur"]
	}



	locs <- data.frame()

	for (k in 1:length(id)) {

		cat("\rExtracting raw locations for:", id[k], "     ")

		query <- paste0(
			"SELECT",
			" * ",
			"FROM",
			" argos_id_colliers ",
			"WHERE",
			" code_couleur = ",
			"'",
			id[k],
			"'"
		)
		ids   <- DBI::dbGetQuery(dbname, query)


		if (nrow(ids) == 0) {

			stop(paste0("The id #", id[k], " is absent from the database."))
		}


		for (j in 1:5) {

			ptt <- ids[1, paste0("ptt_user_", j)]

			if (!is.na(ptt)) {

				query <- paste0(
					"SELECT",
					" * ",
					"FROM",
					" argos_periode_suivi ",
					"WHERE",
					" ptt_user = ",
					"'",
					ptt,
					"'"
				)
				ptts <- DBI::dbGetQuery(dbname, query)

				if (is.null(start) && is.null(end)) {

					if (!is.na(ptts[1, "date_fin_suivi"])) {

						query <- paste0(
							"SELECT",
							" * ",
							"FROM",
							" argos_localisations ",
							"WHERE",
							" platform = ",
							"'",
							ptts[1, "ptt_cls"],
							"'",
							" AND",
							" dateloc >= ",
							"'",
							ptts[1, "date_debut_suivi"],
							"'",
							" AND",
							" dateloc <= ",
							"'",
							ptts[1, "date_fin_suivi"],
							"'"
						)

					} else {

						query <- paste0(
							"SELECT",
							" * ",
							"FROM",
							" argos_localisations ",
							"WHERE",
							" platform = ",
							"'",
							ptts[1, "ptt_cls"],
							"'",
							" AND",
							" dateloc >= ",
							"'",
							ptts[1, "date_debut_suivi"],
							"'"
						)
					}

				} else {

					query <- paste0(
						"SELECT",
						" * ",
						"FROM",
						" argos_localisations ",
						"WHERE",
						" platform = ",
						"'",
						ptts[1, "ptt_cls"],
						"'",
						" AND",
						" dateloc >= ",
						"'",
						start,
						"'",
						" AND",
						" dateloc <= ",
						"'",
						end,
						"'"
					)
				}

				res <- DBI::dbGetQuery(dbname, query)
				res <- data.frame(code_couleur = rep(id[k], nrow(res)), res)
				locs <- rbind(locs, res)
			}
		}
	}

	cat("\n")

	rownames(locs) <- NULL
	return(locs)
}
