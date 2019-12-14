#' @keywords internal

move_stats <- function(data, long = "longitude", lat = "latitude", date = "dateloc"){

	dst <- round(sqrt((data[2, long] - data[1, long])^2 + (data[2, lat] - data[1, lat])^2) / 1000, 2)
	len <- round(difftime(data[2, date], data[1, date],units = "mins")[[1]], 2)
	spd <- round(60 * dst/len, 2)

	return(list(Dist = dst, Duration = len, Speed = spd))
}
