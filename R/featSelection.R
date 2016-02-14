#' featSelection
#' @export

featSelection <- function(dat.all,cls,fs.method,fs.pars,nCores,pw=NULL){   # wrapper function for feature selection, includes parallelisation with both Rmpi and snow
  dn <- names(dat.all)
  lapply(dn, function(i){
    gc()
    cat("\n-Data set = :",i,"\n")
    dat <- dat.all[[i]]
    dat.pair <- dat.sel(dat, cls, choices=pw)
    com      <- apply(dat.pair$com, 1, paste, collapse="~")
    dat.1 <- lapply(com,function(x,dat){
    	data <- data.frame(dat$dat[[x]],dat$cl[[x]])
    	return(data)
    },dat = dat.pair)
    names(dat.1) <- com
    if (nCores<2){
      fs.pair <- lapply(com, function(x,dat) {
        cat("\n--Pairwise = :",x,'\n'); flush.console()
        mfs(dat[[x]],fs.method, fs.pars)
      },dat =dat.1)
    } else {
      clust = makeCluster(nCores,type="PSOCK")
      clusterExport(clust,c(ls("package:FIEmspro"),ls("package:MASS"),ls("package:e1071"),ls("package:randomForest"),ls("package:OrbiFIEmisc")))
      fs.pair <- parLapplyLB(clust, dat.1, fun=mfs,fs.method=fs.method,fs.pars=fs.pars)
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