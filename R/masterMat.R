#' to build mean intensity matrices  by class including nominal and 2dp bins

masterMat <-
function(master_mix,cls){
		mat.p <- master_mix[[1]]
		mat.n <- master_mix[[2]]
		mass.p <- gsub("p","",colnames(mat.p))
		mass.n <- gsub("n","",colnames(mat.n))
		mass.p <- as.numeric(mass.p)
		mass.n <- as.numeric(mass.n)
		mass.p.0 <- round(mass.p,0)
		mass.n.0 <- round(mass.n,0)
		mass.p.1 <- round(mass.p,1)
		mass.n.1 <- round(mass.n,1)
		mass.p.2 <- round(mass.p,2)
		mass.n.2 <- round(mass.n,2)
		master.mat.p <- t(rbind(mass.p.0,mass.p.1,mass.p.2,mass.p,mat.p))
		master.mat.n <- t(rbind(mass.n.0,mass.n.1,mass.n.2,mass.n,mat.n))
		colnames(master.mat.p)[5:ncol(master.mat.p)] <- unique(as.character(cls))
		colnames(master.mat.n)[5:ncol(master.mat.n)] <- unique(as.character(cls))
		master.mat <- list(master.mat.p,master.mat.n)
}
