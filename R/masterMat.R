#' to build mean intensity matrices  by class including nominal and 2dp bins
#' @export

masterMat <-
function(master_mix,cls){
  master.mix <- list()
	for(i in 1:length(master_mix)){
    mat <- master_mix[[i]]
    if (grepl("p",colnames(mat)[1])==T){
		  mass <- gsub("p","",colnames(mat))
		}
		if (grepl("n",colnames(mat)[1])==T){
		  mass <- gsub("n","",colnames(mat))
		}
		mass <- as.numeric(mass)
		mass.0 <- round(mass,0)
		mass.1 <- round(mass,1)
		mass.2 <- round(mass,2)
		master.mat <- t(rbind(mass.0,mass.1,mass.2,mass,mat))
		colnames(master.mat)[5:ncol(master.mat)] <- unique(as.character(cls))
		master.mix[[i]] <- master.mat
	}
  return(master.mix)
}
