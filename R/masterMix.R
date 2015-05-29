#' Build mean intensity matrices  by class.

masterMix <-
function(peak.mat,cls){
	cls.1 <- unique(cls)
	master.mat <- NULL
	for (i in 1:length(peak.mat)){
		mat <- matrix(0,nrow=length(cls.1),ncol=ncol(peak.mat[[i]]))
		for (x in 1:length(cls.1)){
			cls.mat <- subset(peak.mat[[i]],cls==cls.1[x])
			mean.cls <- apply(cls.mat,2,mean)
			mat[x,] <- mean.cls
		}
		colnames(mat) <- colnames(peak.mat[[i]])
		rownames(mat) <- cls.1
		master.mat[i] <- list(mat)
	}
	return(master.mat)
}
