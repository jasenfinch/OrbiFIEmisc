#' builds occupancy matrix by class
#' @export

occMat <-
function(dat,cls){
	cls.1 <- unique(cls)
	mat <- matrix(0,nrow=length(cls.1),ncol=(ncol(dat)))
	for (i in 1:length(cls.1)){
		a <- cls==cls.1[i]
		dat.1 <- dat[a,]
		dat.1[dat.1>0] <- TRUE
		dat.1[dat.1==0] <- FALSE
		tot <- apply(dat.1,2,sum)
		tot <- tot/nrow(dat.1)
		mat[i,] <- tot
	}
#	min.occ <- apply(mat,2,min)
#	mat <- rbind(mat,min.occ)
	colnames(mat) <- colnames(dat)
	return(mat)
}
