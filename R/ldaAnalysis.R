#' wrapper function for LDA plotting, returns a list containg results.

ldaAnalysis <-
function(dat.all,cls,Path,DF,cls.1=NULL,HPC_mode=F){
	dn <- names(dat.all)
	 pcalda <- lapply(dn, function(x) {
    res <- nlda(dat.all[[x]],cls)
    dfs <- as.data.frame(res$x)
    dfs <- cbind(dfs, y=cls,type=rep(x, nrow(dat.all[[x]])))
    list(dfs=dfs, eig=res$Tw)
  })
  names(pcalda) <- dn
  lda.dfs <- do.call(rbind, lapply(pcalda, function(x) x$dfs))
 lda.eig <- do.call(rbind, lapply(pcalda, function(x) x$eig))
 if (length(unique(cls))<3){
 	ldaStrip(lda.dfs,lda.eig,dn,Path,DF,HPC_mode=HPC_mode)
 } else {
  if (length(unique(cls))<4){
    ldaPlots(lda.dfs,lda.eig,"DF1","DF2",dn,Path,DF,cls.1=cls.1,HPC_mode=HPC_mode)
  } else {
 	  ldaPlots(lda.dfs,lda.eig,"DF1","DF2",dn,Path,DF,cls.1=cls.1,HPC_mode=HPC_mode)
 	  ldaPlots(lda.dfs,lda.eig,"DF1","DF3",dn,Path,DF,cls.1=cls.1,HPC_mode=HPC_mode)
 	  ldaPlots(lda.dfs,lda.eig,"DF2","DF3",dn,Path,DF,cls.1=cls.1,HPC_mode=HPC_mode)
  }
 }
 ldaDotplot(lda.eig,dn,Path,DF,HPC_mode=HPC_mode)
 lda.res <- list(lda.dfs=lda.dfs,lda.eig=lda.eig)
 write.table(lda.eig,file=paste(Path,DF,paste(DF,"PCA_&_LDA",sep="_"),paste(DF, "_lda_stats.csv",sep=""),sep="/"),
              sep=",", quote=F,row.names=T,col.names=NA)
 return(lda.res)
}
