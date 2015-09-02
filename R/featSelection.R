featSelection <- function(dat.all,cls,fs.method,fs.pars,nCores,pw=NULL){   # wrapper function for feature selection, includes parallelisation with both Rmpi and snow
  dn <- names(dat.all)
  lapply(dn, function(i){
    gc()
    cat("\n-Data set = :",i,"\n")
    dat <- dat.all[[i]]
    dat.pair <- dat.sel(dat, cls, choices=pw)
    com      <- apply(dat.pair$com, 1, paste, collapse="~")
    dat.1<- NULL
    for (z in 1:length(com)){
      dat.com <- data.frame(dat.pair$dat[z],dat.pair$cl[z])
      names(dat.com) <- names(dat.pair$dat[[z]])
      dat.com <- as.matrix(dat.com)
      dat.1[z] <- list(dat.com)
    }
    names(dat.1) <- com
    if (nCores<2){
      fs.pair <- lapply(dat.1, function(x) {
        cat("\n--Pairwise = :",names(x)); flush.console()
        x <- data.frame(x)
        mfs(x[,1:ncol(x)-1],x[,ncol(x)],fs.method, fs.pars)
      })
    } else {
      clust = makeCluster(nCores,type="PSOCK")
      clusterExport(clust,c(ls("package:FIEmspro"),ls("package:MASS"),ls("package:e1071"),ls("package:randomForest"),ls("package:OrbiFIEmisc")))
      fs.pair <- clusterApplyLB(clust, dat.1, fun=mfs,fs.method=fs.method,fs.pars=fs.pars)
      stopCluster(clust)
    }
    names(fs.pair) <- com
    save(fs.pair, file=paste(i,"fs_re_DO_NOT_DELETE.RData",sep="_"))
  })
  fs.res <- NULL
  for (a in 1:length(dn)){
    load(file=paste(dn[a],"fs_re_DO_NOT_DELETE.RData",sep="_"))
    fs.res[a] <- list(fs.pair)
    unlink(paste(dn[a],"fs_re_DO_NOT_DELETE.RData",sep="_"))
  }
  names(fs.res) <- dn
  return(fs.res)
}