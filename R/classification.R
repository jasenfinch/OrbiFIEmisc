Classification <- function(dat.all,dn,cls,pw,cl.method,cl.pars,nCores,Path,DF,srce){
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
    dat  <- dat.all[[i]]
    cat("\n-Data set = :",i,"\n")
    cat("\n-Data set = :",i,"\n",file=paste(Path,DF,paste(DF,"log-file.txt",sep="_"),sep="/"),append=T)
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
      res.cl <- lapply(dat.1, function(x) {
        cat("\n--Pairwise = :",names(x)); flush.console()
        x <- data.frame(x)
        my.mcl(x[,1:ncol(x)-1],x[,ncol(x)],cl.method, cl.pars)
      })     
    } else {
      library(parallel)	
      clust = makeCluster(nCores, type="PSOCK")
      res.cl <- clusterApplyLB(clust,dat.1,my.mcl.par,cl.method=cl.method,cl.pars=cl.pars,srce=srce)
      stopCluster(clust)
    }
    cat('...  done in ',FIEmspro:::timer_end(time1)$dt,"\n",sep=" ")
    cat('...  done in ',FIEmspro:::timer_end(time1)$dt,"\n",sep=" ",file=paste(Path,DF,paste(DF,"log-file.txt",sep="_"),sep="/"),append=T)
    names(res.cl) <- com
    save(res.cl, file=paste(Path,DF,paste(DF,"Classification_&_Feature_Selection",sep="_"),paste(DF,i,"cl_res_DO_NOT_DELETE.RData",sep="_"),sep="/"))
  })
  aam <- NULL
  for (a in 1:length(dn)){
    load(file=paste(Path,DF,paste(DF,"Classification_&_Feature_Selection",sep="_"),paste(DF,dn[a],"cl_res_DO_NOT_DELETE.RData",sep="_"),sep="/"))
    aam[a] <- list(res.cl)
    unlink(paste(Path,DF,paste(DF,"Classification_&_Feature_Selection",sep="_"),paste(DF,dn[a],"cl_res_DO_NOT_DELETE.RData",sep="_"),sep="/"))
  }
  names(aam) <- dn
  aam <- reFormatClassi(aam)
  save(aam, cl.method, cl.pars, file=paste(Path,DF,paste(DF,"Classification_&_Feature_Selection",sep="_"),paste(DF,"cl_res.RData",sep="_"),sep="/"))
  for (i in 1:length(aam)){
    write.csv(aam[[i]],file=paste(Path,DF,paste(DF,"Classification_&_Feature_Selection",sep="_"),paste(DF,names(aam)[i], "cl_aam.csv",sep="_"),sep="/"),row.names = F)
  }
  return (aam)
}