#' Plot heatmap of log2 ratios treatments 

heatmapRatios <-
function(data,masses,cls.treat,cls.time,rel=T){
    data <- data[,which(names(data) %in% masses)]
    log.data <- logRatio(data,cls.treat,cls.time)
	  if (rel){
		log.data <- apply(log.data,2,scaleSS)
	  }
		heatmap.2(t(log.data),col=redgreen(200),symbreaks=TRUE ,Colv=FALSE,symkey=T,
          main="log2 ratios",xlab="Hours post inoculation",ylab="Explanatory m/z",keysize=1,trace="none",density.info="histogram")
}
