#' wrapper function for principle component analysis, returns a list containg results

pcAnalysis <- function(dat.all,cls){
	dn <- names(dat.all)
	pca.1 <- lapply(dn, function(x) {
     pca  <- pcaComp(dat.all[[x]], scale=F, pcs=1:8)
   	 scores <- cbind(pca$scores, cls=cls,type=rep(x, nrow(dat.all[[x]])))
   	 loadings <- cbind(pca$loadings, type=rep(x, nrow(pca$loadings)))
   	 list(scores=scores, vars=pca$vars,loadings=loadings)
  })
    pca.scores <- do.call(rbind, lapply(pca.1, function(x) x$scores))
    pca.vars   <- do.call(rbind, lapply(pca.1, function(x) x$vars))
    pca.loadings <- do.call(rbind, lapply(pca.1, function(x) x$loadings))
    pca.res <- list(pca.scores=pca.scores, pca.vars=pca.vars, pca.loadings=data.frame(pca.loadings,stringsAsFactors = F))
	return(pca.res)
}
