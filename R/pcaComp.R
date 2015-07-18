

pcaComp <-
function(x, scale=FALSE, pcs=1:2,...)
{
  pca  <- prcomp(x, scale=scale)
  vars <- pca$sdev^2
  vars <- vars/sum(vars)      
  names(vars) <- colnames(pca$rotation)
  vars <- round(vars * 100,2)
  dfn  <- paste(names(vars),": ",vars[names(vars)],"%",sep="") 
  x    <- data.frame(pca$x)

  x    <- x[,pcs]
  vars <- vars[pcs]
  dfn  <- dfn[pcs]
  loadings <- pca$rotation
  loadings <- loadings[,pcs]
  list(scores=x,vars=vars,varsn=dfn,loadings=loadings)
}
