pcalda <-
function(dat.all,dn,cls){
 pcalda <- lapply(dn, function(x) {
    res <- nlda(dat.all[[x]],cls)
    dfs <- as.data.frame(res$x)
    dfs <- cbind(dfs, y=cls,type=rep(x, nrow(dat.all[[x]])))
    list(dfs=dfs, eig=res$Tw)
  })
  names(pcalda) <- dn
 return(pcalda)
}
