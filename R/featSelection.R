featSelection <- function(dat.all,dn,cls,pw,fs.method,fs.pars,nCores,Path,DF,srce){   # wrapper function for feature selection, includes parallelisation with both Rmpi and snow
  if(!file.exists(paste(Path,DF,paste(DF,"Classification_&_Feature_Selection",sep="_"),sep="/"))){
    dir.create(paste(Path,DF,paste(DF,"Classification_&_Feature_Selection",sep="_"),sep="/"))
  }
  ncls = length(unique(cls))
  seq.1 = seq(1,ncls-1)
  npair = sum(seq.1)
  cat(paste("No. classes:",ncls,"\n",sep=" "))
  cat(paste("No. classes:",ncls,"\n",sep=" "),file=paste(Path,DF,paste(DF,"log-file.txt",sep="_"),sep="/"),append=T)
  cat(paste("No. pairwise comparisons:", npair,"\n", sep=" "))
  cat(paste("No. pairwise comparisons:", npair,"\n", sep=" "),file=paste(Path,DF,paste(DF,"log-file.txt",sep="_"),sep="/"),append=T)
  lapply(dn, function(i){
    gc()
    cat("\n-Data set = :",i,"\n")
    cat("\n-Data set = :",i,"\n",file=paste(Path,DF,paste(DF,"log-file.txt",sep="_"),sep="/"),append=T)
    time1 <- FIEmspro:::timer_start()
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
        my.mfs(x[,1:ncol(x)-1],x[,ncol(x)],fs.method, fs.pars)
      })
    } else {
      library(parallel)
      clust = makeCluster(nCores,type="PSOCK")
      fs.pair <- clusterApplyLB(clust, dat.1, fun=my.mfs.par,fs.method.1=fs.method,fs.pars.1=fs.pars,srce.1=srce)
      stopCluster(clust)
      cat('... done in ',FIEmspro:::timer_end(time1)$dt,"\n",sep="")
      cat('... done in ',FIEmspro:::timer_end(time1)$dt,"\n",sep="",file=paste(Path,DF,paste(DF,"log-file.txt",sep="_"),sep="/"),append=T)
    }
    names(fs.pair) <- com
    fs.pair <- lapply(fs.pair,function(x){return(x[-which(names(x)=="all")])})
    save(fs.pair, file=paste(Path,DF,paste(DF,"Classification_&_Feature_Selection",sep="_"),paste(DF,i,"fs_re_DO_NOT_DELETE.RData",sep="_"),sep="/"))
  })
  fs.res <- NULL
  for (a in 1:length(dn)){
    load(file=paste(Path,DF,paste(DF,"Classification_&_Feature_Selection",sep="_"),paste(DF,dn[a],"fs_re_DO_NOT_DELETE.RData",sep="_"),sep="/"))
    fs.res[a] <- list(fs.pair)
    unlink(paste(Path,DF,paste(DF,"Classification_&_Feature_Selection",sep="_"),paste(DF,dn[a],"fs_re_DO_NOT_DELETE.RData",sep="_"),sep="/"))
  }
  names(fs.res) <- dn
  save(fs.res, file=paste(Path,DF,paste(DF,"Classification_&_Feature_Selection",sep="_"),paste(DF,"fs_re.RData",sep="_"),sep="/"))
  fs.order  <- lapply(fs.res, function(x) lapply(x, function(y) y$fs.order))
  fs.rank  <- lapply(fs.res, function(x) lapply(x, function(y) y$fs.rank))
  fs.ord   <- lapply(fs.rank, function(x) lapply(x, function(y) fs.agg(y)$fs.order))
  stats.1 <- fsTab(fs.res)
  for(i in 1:length(stats.1)){
    write.csv(stats.1[[i]],file=paste(Path,DF,paste(DF,"Classification_&_Feature_Selection",sep="_"),paste(DF,names(stats.1)[i], "fs_stats_agg.csv",sep="_"),sep="/"),row.names = F)
  }
  return(fs.res)
}