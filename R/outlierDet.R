

outlierDet <-
function(dat.all,samp.names,HPC_mode=F){
  dat.p   <- prcomp(dat.all[["Positive_Mode"]],scale=FALSE)$x
  dat.n   <- prcomp(dat.all[["Negative_Mode"]],scale=FALSE)$x
  if (HPC_mode==T){
  	bitmap(paste(Path,DF,paste(DF,names(dat.all)[1],"outler_detection.bmp",sep="_"),sep="/"))
  } else {
  	jpeg(paste(Path,DF,paste(DF,names(dat.all)[1],"outler_detection.jpg",sep="_"),sep="/"))
  } 
  p <- outDet(dat.p[,c(1,2)],samp.names, method="mcd",dimen=c(1,2),conf.level = 0.975)
  dev.off()
  if (HPC_mode==T){
  	bitmap(paste(Path,DF,paste(DF,names(dat.all)[2],"outlier_detection.bmp",sep="_"),sep="/"))
  } else {
  	jpeg(paste(Path,DF,paste(DF,names(dat.all)[2],"outlier_detection.jpg",sep="_"),sep="/"))
  }
  n <- outDet(dat.n[,c(1,2)],samp.names, method="mcd",dimen=c(1,2),conf.level = 0.975)
  dev.off()
  return(list(pos=p,neg=n))
}
