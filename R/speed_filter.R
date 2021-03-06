#' @title Remove Argos location based on speed characteristics
#' @description Remove Argos location based on speed characteristics.
#'
#' @param data Cleaned Argos location data frame
#' @param selqual Location qualities to keep
#' @param speed1 Lower speed
#' @param speed2 Upper speed
#' @param timespeed2 Duration of upper speed
#' @param noid Column name of animal id
#' @param date Column name of location date
#' @param long Column name of longitude
#' @param lat Column name of latitude
#' @param qual Column name of location quality
#' @param crs proj4string of UTM zone of study area
#'
#' @return A data frame with 12 variables
#'
#' @importFrom rgdal project
#'
#' @export
#'
#' @examples
#' ## See vignette
#'


speed_filter <- function(
	data, selqual = c(0, 1, 2, 3), speed1 = 7, speed2 = 10, timespeed2 = 12,
	noid = 'noid', date = 'dateloc', long = 'longitude', lat = 'latitude',
	qual = 'quality', crs = "+proj=utm +zone=17 +datum=NAD83 +ellps.default=GCS") {

	data$key <- paste(data[ , noid], data[ , "platform"], sep = "__")

	data[ , "key"] <- as.character(data[ , "key"])

	data <- data[order(data[ , "key"], data[ , date]),]
	ids  <- levels(as.factor(data[ , "key"]))

	xy <- rgdal::project(as.matrix(data[ , c(long, lat)]), proj = crs)
	data <- data.frame(data, long_p = xy[ , 1], lat_p = xy[ , 2])

	TAB <- NULL

	for (j in 1 : length(ids)) {

		print(paste("Filtering ",ids[j], "...", sep = ""))

		tab <- data[data[ , "key"] == ids[j], ]

		if (!is.null(selqual)) tab <- tab[tab[ , qual] %in% selqual, ]

		tab <- tab[order(tab[ , date]), ]
		rownames(tab) <- NULL

		tab$StepLen <- NA
		tab$DiffTim <- NA
		tab$Speed   <- NA

		i <- 2

		while (i <= nrow(tab)) {

			if (i == 1) {

				tab[1, "StepLen"] <- NA
				tab[1, "DiffTim"] <- NA
				tab[1, "Speed"]   <- NA

				i <- 2
			}

			param <- move_stats(tab[c(i - 1, i),], long = 'long_p', lat = 'lat_p', date = date)

			tab[i, "StepLen"] <- param$"Dist"
			tab[i, "DiffTim"] <- param$"Duration"
			tab[i, "Speed"]   <- param$"Speed"

			if (tab[i, "Speed"] > speed1) {

				if (tab[i, "Speed"] <= speed2 && tab[i, "DiffTim"] <= timespeed2) {

					beg.day <- min(which(substr(tab[ , date], 1, 10) == substr(tab[i, date], 1, 10)))

					if (beg.day == i && (tab[substr(tab[i, date], 1, 10) != substr(tab[i+1, date], 1, 10), ])) {

						if (i >= 4) {

							if ((i + 3) <= nrow(tab)) {

								vec <- seq(i - 3, i + 3)

							} else {

								vec <- seq(i - 3, nrow(tab))
							}

						} else {

							if ((i+3) <= nrow(tab)) {

								vec <- seq(1, i + 3)

							} else {

								vec <- seq(1, nrow(tab))
							}
						}

						Bar <- barycentre(tab[vec, c('long_p', 'lat_p')])
						dd  <- apply(tab[vec, c('long_p', 'lat_p')], 1, G = Bar, euclidean_dist)

						if (vec[which.max(dd)] == beg.day) {

							tab <- tab[-beg.day, ]
							rownames(tab) <- NULL
							i <- beg.day

						} else {

							tab <- tab[-i,]
							rownames(tab) <- NULL
						}

					} else {
						i <- i + 1
					}

				} else {

					beg.day <- min(which(substr(tab[ , date], 1, 10) == substr(tab[i, date], 1, 10)))

					if (beg.day == i) {

						tab <- tab[-i, ]
						rownames(tab) <- NULL

					} else {

						if ((i - beg.day) <= 3) {

							if (i >= 4) {

								if ((i + 3) <= nrow(tab)) {

									vec <- seq(i - 3, i + 3)

								} else {

									vec <- seq(i - 3, nrow(tab))
								}

							} else {

								if ((i + 3) <= nrow(tab)) {

									vec <- seq(1, i + 3)

								} else {

									vec <- seq(1, nrow(tab))
								}
							}

							Bar <- barycentre(tab[vec, c('long_p', 'lat_p')])
							dd <- apply(tab[vec, c('long_p', 'lat_p')], 1, G = Bar, euclidean_dist)

							if (vec[which.max(dd)] == beg.day) {

								tab <- tab[-beg.day, ]
								rownames(tab) <- NULL

							} else {

								tab <- tab[-i, ]
								rownames(tab) <- NULL
							}

						} else {

							tab <- tab[-i, ]
							rownames(tab) <- NULL
						}
					}
				}

			} else {

				i <- i + 1
			}

			if (i > nrow(tab)) break
		}

		rownames(tab) <- NULL
		TAB <- rbind(TAB, tab)
	}

	return(TAB)
}
