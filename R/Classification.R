Classification <- function(dat.all,cls,cl.method,cl.pars,nCores,pw=NULL){
  dn <- names(dat.all)
  lapply(dn, function(i){
    dat  <- dat.all[[i]]
    cat("\n-Data set = :",i,"\n")
    time1 <- FIEmspro:::timer_start()
    dat.pair <- dat.sel(dat, cls, choices=pw)
    com <- apply(dat.pair$com, 1, paste, collapse="~")
    dat.1<- NULL
    for (z in 1:length(com)){
      dat.com <- data.frame(dat.pair$dat[z],dat.pair$cl[z])
      names(dat.com) <- names(dat.pair$dat[[z]])
      dat.com <- data.frame(dat.com)
      dat.1[z] <- list(dat.com)
    }
    names(dat.1) <- com
    if (nCores <2){
      res.cl <- lapply(names(dat.1), function(x,dat) {
        cat("\n--Pairwise = :",x); flush.console()
        dat <- dat.1[[x]]
        runClassification(dat[[x]],cl.method, cl.pars)
      },dat=dat.1)     
    } else {
      library(parallel)	
      clust = makeCluster(nCores, type="PSOCK")
      clusterExport(clust,c(ls("package:FIEmspro"),ls("package:MASS"),ls("package:e1071"),ls("package:randomForest"),ls("package:OrbiFIEmisc")))
      res.cl <- clusterApplyLB(clust,dat.1,runClassification,cl.method=cl.method,cl.pars=cl.pars)
      stopCluster(clust)
    }
    cat('...  done in ',FIEmspro:::timer_end(time1)$dt,"\n",sep=" ")
    names(res.cl) <- com
    save(res.cl, file=paste(i,"cl_res_DO_NOT_DELETE.RData",sep="_"))
  })
  aam <- NULL
  for (a in 1:length(dn)){
    load(file=paste(dn[a],"cl_res_DO_NOT_DELETE.RData",sep="_"))
    aam[a] <- list(res.cl)
    unlink(paste(dn[a],"cl_res_DO_NOT_DELETE.RData",sep="_"))
  }
  names(aam) <- dn
  aam <- reFormatClassi(aam)
  return (aam)
}