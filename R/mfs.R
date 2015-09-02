mfs <- function(x, fs.method,fs.pars,...){
  x <- data.frame(x)
  res <- lapply(fs.method, function(m) {
    featRank(x[,1:ncol(x)-1],x[,ncol(x)],method=m,pars=fs.pars)
  })
  fs.stats    <- as.data.frame(sapply(res,function(x) x$fs.stats))
  names(fs.stats) <- fs.method
  fs.pval <- sapply(res,function(x){if(length(x)>0){return(x$fs.pval)}})
  names(fs.pval) <- fs.method
  fs.pval <- as.data.frame(fs.pval[-which(sapply(fs.pval,length)==0)])
  fs.tab <- featTab(fs.stats,fs.pval)
  names(fs.tab) <- fs.method
  fs.tab <- as.data.frame(fs.tab)
  return(fs.tab)
}