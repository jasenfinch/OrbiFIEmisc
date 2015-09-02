mfs <- function(x, fs.method,fs.pars,...){
  x <- data.frame(x)
  res <- lapply(fs.method, function(m) {
    featRank(x[,1:ncol(x)-1],x[,ncol(x)],method=m,pars=fs.pars,...)
  })
  names(res)  <- fs.method
  fs.rank     <- sapply(res,function(x) x$fs.rank)
  fs.stats    <- sapply(res,function(x) x$fs.stats)
  fs.tab      <- featTab(fs.stats)
  fs.order    <- sapply(res, function(x) x$fs.order)
  list(fs.order=fs.order, fs.rank=fs.rank, fs.stats=fs.stats, fs.tab=fs.tab)
}