

processDat <-
function(peak.mat,cls,bat,out_id,out_idx,par.m,nCores,TIC_cls=NULL){
	if (!(par.m)){
		dat.all <- lapply(peak.mat,preProcess,cls=cls,bat=bat,out_id=out_id,out_idx=out_idx,TIC_cls=TIC_cls)
		
	} else {
		if (nCores>2){
			nCores <- 2
		}
    clust = makeCluster(nCores, type="PSOCK")
    clusterExport(clust,c(ls("package:FIEmspro"),ls("package:OrbiFIEmisc")))
		dat.all <- clusterApplyLB(clust,peak.mat, fun=preProcess,cls=cls,bat=bat,out_id=out_id,out_idx=out_idx,TIC_cls=TIC_cls)
    stopCluster(clust)
	}
	return(dat.all)
}
