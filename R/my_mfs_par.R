my.mfs.par <- function(x, fs.method.1=fs.method, fs.pars.1=fs.pars, srce.1=srce,...){
  x <- data.frame(x)
  library(FIEmspro)
  source(srce.1)
  res <- lapply(fs.method.1, function(m) {
    feat.rank.par(x[,1:ncol(x)-1],x[,ncol(x)],method=m,pars=fs.pars.1,...)
  })
  names(res)  <- fs.method.1
  fs.rank     <- sapply(res,function(x) x$fs.rank)
  fs.stats    <- sapply(res,function(x) x$fs.stats)
  fs.tab      <- feat.tab(fs.stats)
  fs.order    <- sapply(res, function(x) x$fs.order)
  list(fs.order=fs.order, fs.rank=fs.rank, fs.stats=fs.stats, fs.tab=fs.tab,
       all=res)
}