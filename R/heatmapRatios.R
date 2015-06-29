#' Plot heatmap of log2 ratios treatments 

heatmapRatios <-
function(dat.all,masses,cls.treat,cls.time,rel=T){
  for (i in names(dat.all)){
    data <- dat.all[[i]]
    data <- data[,which(names(data) %in% masses[i])]
    log.data <- logRatio(data,cls.treat,cls.time)
	  if (rel){
		log.data <- apply(log.data,2,scaleSS)
	  }
		heatmap.2(t(log.data),col=redgreen(200),symbreaks=TRUE ,Colv=FALSE,symkey=T,
          main=paste(i,"log2 I:M ratios",sep=" "),xlab="Hours post inoculation",ylab="Explanatory m/z",keysize=1,trace="none",density.info="histogram")
  }
}
