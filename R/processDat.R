

processDat <-
function(peak.mat,cls=NULL,bat=NULL,out_id=NULL,out_idx=NULL,nCores=2,TIC_cls=NULL,procmeth=c("log10","TICnorm"),pre_SD=T,mean_shift=T){
	if (nCores<2){
		dat.all <- lapply(peak.mat,preProcess,cls=cls,bat=bat,out_id=out_id,out_idx=out_idx,TIC_cls=TIC_cls,procmeth=procmeth,pre_SD=pre_SD,mean_shift=mean_shift)
		
	} else {
		if (nCores>2){
			nCores <- 2
		}
    clust = makeCluster(nCores, type="PSOCK")
    clusterExport(clust,c(ls("package:FIEmspro"),ls("package:OrbiFIEmisc")))
		dat.all <- clusterApplyLB(clust,peak.mat, fun=preProcess,cls=cls,bat=bat,out_id=out_id,out_idx=out_idx,TIC_cls=TIC_cls,procmeth=procmeth,pre_SD=pre_SD,mean_shift=mean_shift)
    stopCluster(clust)
	}
	return(dat.all)
}
