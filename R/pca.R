pca <-
function(dat.all,dn,cls){
	pca.1 <- lapply(dn, function(x) {
    	pca  <- pca.comp(dat.all[[x]], scale=F, pcs=1:8)
   	 scores <- cbind(pca$scores, y=cls,type=rep(x, nrow(dat.all[[x]])))
   	 list(scores=scores, vars=pca$vars)
  })
  names(pca.1) <- dn
	return(pca.1)
}
