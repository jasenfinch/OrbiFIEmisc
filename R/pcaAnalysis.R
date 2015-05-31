#' wrapper function for PCA plotting, returns a list containg results

pcaAnalysis <- function(dat.all,cls,Path,DF,cls.1=NULL,HPC_mode=F){
	dn <- names(dat.all)
	pca.1 <- lapply(dn, function(x) {
    	pca  <- pcaComp(dat.all[[x]], scale=F, pcs=1:8)
   	 scores <- cbind(pca$scores, y=cls,type=rep(x, nrow(dat.all[[x]])))
   	 list(scores=scores, vars=pca$vars)
  })
    pca.scores <- do.call(rbind, lapply(pca.1, function(x) x$scores))
    pca.vars   <- do.call(rbind, lapply(pca.1, function(x) x$vars))
    write.table(pca.vars,file=paste(Path,DF,paste(DF,"PCA_&_LDA",sep="_"),paste(DF, "_pca_stats.csv",sep=""),sep="/"),
           sep=",", quote=F,row.names=T,col.names=NA)
  
    pcaPlots(pca.scores,pca.vars,"PC1","PC2",dn,Path,DF,cls.1=cls.1,HPC_mode=HPC_mode)
    pcaPlots(pca.scores,pca.vars,"PC1","PC3",dn,Path,DF,cls.1=cls.1,HPC_mode=HPC_mode)
    pcaPlots(pca.scores,pca.vars,"PC2","PC3",dn,Path,DF,cls.1=cls.1,HPC_mode=HPC_mode)

    pcaDotplot(pca.vars,dn,Path,DF,HPC_mode=HPC_mode)
	pca.res <- list(pca.scores=pca.scores, pca.vars=pca.vars)
	return(pca.res)
}
