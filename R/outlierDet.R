#' outlierDet
#' @export

outlierDet <-
function(dat.all,samp.names,Path,DF){
  dat.p   <- prcomp(dat.all[["Positive_Mode"]],scale=FALSE)$x
  dat.n   <- prcomp(dat.all[["Negative_Mode"]],scale=FALSE)$x
  bitmap(paste(Path,DF,paste(DF,"Positive_Mode","outlier_detection.bmp",sep="_"),sep="/"))
  p <- outDet(dat.p[,c(1,2)],samp.names, method="mcd",dimen=c(1,2),conf.level = 0.975)
  dev.off()
  bitmap(paste(Path,DF,paste(DF,"Negative_Mode","outlier_detection.bmp",sep="_"),sep="/"))
  n <- outDet(dat.n[,c(1,2)],samp.names, method="mcd",dimen=c(1,2),conf.level = 0.975)
  dev.off()
  return(list(pos=p,neg=n))
}
