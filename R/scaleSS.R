#' to scale varibles to their sum of squares

scaleSS <-
function(data){		
	data.1 <- NULL
	norm.dat <- NULL
	norm.dat.1 <- NULL
	data.s <- data^2
	data.sum <- apply(data.s,1,sum)
	data.sum <- sqrt(data.sum)
	for (i in 1:nrow(data)) {
		data.1 <- data[i,]
		for (x in 1:length(data.1)) {
			norm.dat[x] <- data.1[x]/data.sum[i]
			}
		norm.dat.1[i] <- data.frame(norm.dat)
	}
	norm.dat.2 <- data.frame(norm.dat.1)
	colnames(norm.dat.2) <- rownames(data)
	rownames(norm.dat.2) <- colnames(data)
	norm.dat.2 <- t(norm.dat.2)
	return(norm.dat.2)
}
