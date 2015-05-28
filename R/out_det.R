out_det <-
function(dat.all,samp.names,HPC_mode=F){
  dat.p   <- prcomp(dat.all[[1]],scale=FALSE)$x
  dat.n   <- prcomp(dat.all[[2]],scale=FALSE)$x
  if (HPC_mode==T){
  	bitmap(paste(Path,DF,paste(DF,names(dat.all)[1],"outler_detection.bmp",sep="_"),sep="/"))
  } else {
  	jpeg(paste(Path,DF,paste(DF,names(dat.all)[1],"outler_detection.jpg",sep="_"),sep="/"))
  } 
  p <- out.det(dat.p[,c(1,2)],samp.names, method="mcd",dimen=c(1,2),conf.level = 0.975)
  dev.off()
  cat("Positive Mode: ",paste(p$outlier,collapse=", "),"\n",sep="")
  cat("Positive Mode: ",paste(p$outlier,collapse=", "),"\n",sep="",file=paste(Path,DF,paste(DF,"log-file.txt",sep="_"),sep="/"),append=T)
  if (HPC_mode==T){
  	bitmap(paste(Path,DF,paste(DF,names(dat.all)[2],"outlier_detection.bmp",sep="_"),sep="/"))
  } else {
  	jpeg(paste(Path,DF,paste(DF,names(dat.all)[2],"outlier_detection.jpg",sep="_"),sep="/"))
  }
  n <- out.det(dat.n[,c(1,2)],samp.names, method="mcd",dimen=c(1,2),conf.level = 0.975)
  dev.off()
  cat("Negative Mode: ",paste(n$outlier,collapse=", "),"\n",sep="")
  cat("Negative Mode: ",paste(n$outlier,collapse=", "),"\n",sep="",file=paste(Path,DF,paste(DF,"log-file.txt",sep="_"),sep="/"),append=T)
}
