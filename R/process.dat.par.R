process.dat.par <-
function(peak.mat,cls,bat,out_id,out_idx,par.m,srce,nCores){
	if (!(par.m)){
		dat.all <- lapply(peak.mat,pre.process,cls.1=cls,bat.1=bat,srce.1=srce,out_id.1=out_id,out_idx.1=out_idx)
		
	} else {
		library(parallel)
		if (nCores>2){
			nCores <- 2
		}
    	clust = makeCluster(nCores, type="PSOCK")
		dat.all <- clusterApplyLB(clust,peak.mat, fun=pre.process,cls.1=cls,bat.1=bat,srce.1=srce,out_id.1=out_id,out_idx.1=out_idx)
    	stopCluster(clust)
	}
	names(dat.all) <- c("Positive_Mode","Negative_Mode")
	return(dat.all)
}
