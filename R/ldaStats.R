

ldaStats <-
function(dn,dat.all,cls,Path,DF){
    pcalda <- lapply(dn, function(x) {
      res <- nlda(dat.all[[x]],cls)
      dfs <- as.data.frame(res$x)
      dfs <- cbind(dfs, y=cls,type=rep(x, nrow(dat.all[[x]])))
      list(dfs=dfs, eig=res$Tw)
    })
    names(pcalda) <- dn
    
    lda.dfs <- do.call(rbind, lapply(pcalda, function(x) x$dfs))
    lda.eig <- do.call(rbind, lapply(pcalda, function(x) x$eig))
    write.table(lda.eig,file=paste(Path,DF,paste(DF,"PCA_&_LDA",sep="_"),paste(DF, "lda_stats_full.csv",sep="_"),sep="/"),
                sep=",", quote=F,row.names=T,col.names=NA)
}
