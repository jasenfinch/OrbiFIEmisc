occ.drop <-
function(peak.mat,cls,perc){	# drop variables with occupancy below percentage threshold
	dat.1 <- NULL
	for (i in 1:length(peak.mat)){
	mat <- peak.mat[[i]]
	mat.1 <- occ.mat(mat,cls)
	min.occ <- apply(mat.1,2,min)
	dat <- mat[,min.occ>perc]
	dat.1[i] <- list(dat)
	}
	names(dat.1) <- names(peak.mat)
	return(dat.1)
}
