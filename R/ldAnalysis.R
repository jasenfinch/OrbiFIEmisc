#' wrapper function for LDA plotting, returns a list containg results.
#' @export

ldAnalysis <-
function(dat.all,cls){
	dn <- names(dat.all)
  pcalda <- lapply(dn, function(x) {
    res <- nlda(dat.all[[x]],cls)
    dfs <- as.data.frame(res$x)
    dfs <- cbind(dfs, y=cls,type=rep(x, nrow(dat.all[[x]])))
    loadings <- cbind(res$loadings,type=rep(x,nrow(res$loadings)))
    list(dfs=dfs, eig=res$Tw,loadings=loadings)
  })
  names(pcalda) <- dn
  lda.dfs <- do.call(rbind, lapply(pcalda, function(x) x$dfs))
  lda.eig <- do.call(rbind, lapply(pcalda, function(x) x$eig))
  lda.loadings <- do.call(rbind, lapply(pcalda, function(x) x$loadings))
  lda.res <- list(lda.dfs=lda.dfs,lda.eig=lda.eig,lda.loadings=data.frame(lda.loadings,stringsAsFactors = F))
  return(lda.res)
}
