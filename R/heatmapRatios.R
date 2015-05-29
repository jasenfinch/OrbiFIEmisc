

heatmapRatios <-
function(dat.all,dn,masses,info,rel=F){
	dat.all.one <- datAllComb(dat.all,dn)
	dat.all.one <- data.frame(dat.all.one)
	names(dat.all.one) <- gsub("pn.","",names(dat.all.one))
	data <- NULL
	for (i in 1:length(masses)){
	a <- unlist(masses[i],use.names=FALSE)
	dat <- pick(dat.all.one,a) 
	data[i] <- list(dat)
	}
	log.data <- lapply(data,logRatio,info)
	if (rel){
		log.data <- lapply(log.data,scaleSS)
	}
	names(log.data) <- dn
	for (i in 1:length(log.data)){
		data.1 <- data.frame(log.data[i])
		names(data.1) <- sort(unique(info$tp))
		X11()
		heatmap.2(as.matrix(data.1),col=redgreen(200),symbreaks=TRUE ,Colv=FALSE,symkey=T,
          main=paste(names(log.data)[i],"log2 I:M ratios",sep=" "),xlab="Hours post inoculation",ylab="Explanatory m/z",keysize=1,trace="none",density.info="histogram")
	}
}
