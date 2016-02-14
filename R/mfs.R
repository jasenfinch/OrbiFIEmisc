#' mfs
#' @export

mfs <- function(x, fs.method,fs.pars){
  res <- lapply(fs.method, function(m,p,dat) {
    featRank(dat[,1:ncol(dat)-1],dat[,ncol(dat)],method=m,pars=p)
  },p=fs.pars,dat=x)
  fs.stats    <- as.data.frame(sapply(res,function(x) x$fs.stats))
  names(fs.stats) <- fs.method
  fs.pval <- sapply(res,function(x){if(length(x)>0){return(x$fs.pval)}})
  names(fs.pval) <- fs.method
  fs.pval <- as.data.frame(fs.pval[-which(sapply(fs.pval,length)==0)])
  fs.tab <- featTab(fs.stats,fs.pval)
  names(fs.tab) <- fs.method
  fs.tab <- as.data.frame(fs.tab)
  Forest_K <- sapply(res,function(x){if(length(x)>0){return(x$Forest_K)}})
  names(Forest_K) <- fs.method
  return(list(stats=fs.tab,Forest_K=Forest_K))
}