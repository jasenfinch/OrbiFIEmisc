#' mfs
#' @export

mfs <- function(x, fs.method,fs.pars){
  res <- lapply(fs.method, function(m,p,dat) {
    featRank(dat[,1:ncol(dat)-1],dat[,ncol(dat)],method=m,pars=p)
  },p=fs.pars,dat=x)
  fs.stats    <- as.data.frame(sapply(res,function(x) x$fs.stats))
  names(fs.stats) <- fs.method
  fs.pval <- sapply(res,function(x){x$fs.pval})
  nullPval <- which(sapply(fs.pval,length)==0)
  fs.pval <- fs.pval[-nullPval]
  fs.pval <- as.data.frame(fs.pval)
  names(fs.pval) <- fs.method[-nullPval]
  fs.tab <- featTab(fs.stats,fs.pval)
  names(fs.tab) <- fs.method
  fs.tab <- as.data.frame(fs.tab)
  return(fs.tab)
}