pca.stats <-
function(dn,dat.all,cls,Path,DF){
  if(!file.exists(paste(Path,DF,paste(DF,"PCA_&_LDA",sep="_"),sep="/"))){
    dir.create(paste(Path,DF,paste(DF,"PCA_&_LDA",sep="_"),sep="/"))
  }
	pca <- lapply(dn, function(x) {
      pca  <- pca.comp(dat.all[[x]], scale=F, pcs=1:3)
      scores <- cbind(pca$scores, y=cls,type=rep(x, nrow(dat.all[[x]])))
      list(scores=scores, vars=pca$vars)
    })
    names(pca) <- dn
    
    pca.scores <- do.call(rbind, lapply(pca, function(x) x$scores))
    pca.vars   <- do.call(rbind, lapply(pca, function(x) x$vars))
    write.table(pca.vars,file=paste(Path,DF,paste(DF,"PCA_&_LDA",sep="_"),paste(DF, "pca_stats_full.csv",sep="_"),sep="/"),
                sep=",", quote=F,row.names=T,col.names=NA)
}
