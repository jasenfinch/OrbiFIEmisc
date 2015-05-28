#' wrapper function for LDA plotting, returns a list containg results.

ldaAnalysis <-
function(dat.all,cls,Path,DF,cls.1=NULL,HPC_mode=F){
	suppressMessages(library(ggplot2))
	library(reshape2)
	if(!file.exists(paste(Path,DF,paste(DF,"PCA_&_LDA",sep="_"),sep="/"))){
	  dir.create(paste(Path,DF,paste(DF,"PCA_&_LDA",sep="_"),sep="/"))
	}
	dn <- names(dat.all)
	pcalda.1 <- pcalda(dat.all,dn,cls)
  lda.dfs <- do.call(rbind, lapply(pcalda.1, function(x) x$dfs))
 lda.eig <- do.call(rbind, lapply(pcalda.1, function(x) x$eig))
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
