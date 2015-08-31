my.mcl.par <- function(x,cl.method.1=cl.method, cl.pars.1=cl.pars,srce.1=srce,...) {
  x <- data.frame(x)
  library(FIEmspro)
  source(srce.1)
  res <- lapply(cl.method.1, function(m) {
    my.cl(as.matrix(x[,1:ncol(x)-1]),x[,ncol(x)],cl.method=m, cl.pars.1,...)
  })
  names(res) <- cl.method.1
  res <- do.call(rbind, res)
}