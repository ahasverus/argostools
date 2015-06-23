temporalbuffer <-
function(data, interval = 5, noid = "noid", date = "dateloc", qual = "quality", semmajaxis = "semmajaxis"){
	
	data <- data[do.call(order, data[ , c(noid, date)]), ]
	rownames(data) <- NULL
   
	if (!is.null(interval)){

		res <- data.frame()
		temp <- data
   
		for (k in 1 : length(levels(as.factor(temp[ , noid])))){
      
			print(paste("Filtering ",levels(as.factor(temp[ , noid]))[k], "...", sep = ""))
   
			data <- temp[temp[ , noid] == levels(as.factor(temp[ , noid]))[k],]

			i <- 1
			while (i < nrow(data)){
				
				j <- i ; vec <- NULL
         
				while (difftime(data[j+1, date], data[j, date], units = "mins")[[1]] < interval && data[j+1, noid] == data[j, noid] && j <= nrow(data)-1){
         
					vec <- c(vec, j, j+1)
					j <- j + 1
					
					if (j >= nrow(data)) break
				}

				if (length(vec) > 0) 
					vec <- as.numeric(levels(as.factor(vec)))

				if (j > i){
					
					pos <- which(is.na(data[vec, semmajaxis]))
					
					if (length(pos) > 0)
						ind <- ifelse(length(vec[-pos]) > 0, which.min(data[vec, semmajaxis]), 0)

					if (length(pos) == 0)
						ind <- which.min(data[vec, semmajaxis])

					if (ind == 0){
						ind <- which(data[vec, qual] == "3")

						if (length(ind) == 0){
							ind <- which(data[vec, qual] == "2")
							
							if (length(ind) == 0){
								ind <- which(data[vec, qual] == "1")
							
								if (length(ind) == 0){
									ind <- which(data[vec, qual] == "0")

									if (length(ind) == 0){
										ind <- which(data[vec, qual] == "A")

										if (length(ind) == 0){
											ind <- which(data[vec, qual] == "B")

											if (length(ind) == 0){
												ind <- which(data[vec, qual] == "Z")
											}
										}
									}
								}
							}
						}
					}

					data <- data[-vec[-ind[1]], ]
					rownames(data) <- NULL
				}

				i <- i + 1
				if (i >= nrow(data)) break
			}
			
			rownames(data) <- NULL
			res <- rbind(res, data)
		}

		data <- res
	}

	return(data)
}
