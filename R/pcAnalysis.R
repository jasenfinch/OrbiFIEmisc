#' wrapper function for principle component analysis, returns a list containg results
#' @export

pcAnalysis <- function(dat.all,cls){
	dn <- names(dat.all)
	pca.1 <- lapply(dn, function(x) {
     pca  <- pcaComp(dat.all[[x]], scale=F, pcs=1:8)
   	 scores <- data.frame(pca$scores, cls=cls,type=rep(x, nrow(dat.all[[x]])))
   	 loadings <- data.frame(pca$loadings, type=rep(x, nrow(pca$loadings)))
   	 list(scores=scores, vars=pca$vars,loadings=loadings)
  })
    pca.scores <- do.call(rbind, lapply(pca.1, function(x) x$scores))
    pca.vars   <- do.call(rbind, lapply(pca.1, function(x) x$vars))
    rownames(pca.vars) <- dn
    pca.loadings <- ldply(lapply(pca.1, function(x){return(x$loadings)}),data.frame)
    pca.res <- list(pca.scores=pca.scores, pca.vars=pca.vars, pca.loadings=data.frame(pca.loadings,stringsAsFactors = F))
	return(pca.res)
}
