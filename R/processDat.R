

processDat <-
function(peak.mat,cls,bat,out_id,out_idx,par.m,nCores){
	if (!(par.m)){
		dat.all <- lapply(peak.mat,preProcess,cls.1=cls,bat.1=bat,out_id.1=out_id,out_idx.1=out_idx)
		
	} else {
		if (nCores>2){
			nCores <- 2
		}
    clust = makeCluster(nCores, type="PSOCK")
    clusterExport(clust,ls("package:FIEmspro"))
		dat.all <- clusterApplyLB(clust,peak.mat, fun=preProcess,cls.1=cls,bat.1=bat,out_id.1=out_id,out_idx.1=out_idx)
    stopCluster(clust)
	}
	return(dat.all)
}
