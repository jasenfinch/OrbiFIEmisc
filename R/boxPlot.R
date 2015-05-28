
boxPlot <-
function(x,info,Path,DF,masses,dn){ 
	for(i in 1:length(x)){
	dat.mat <- x[[i]]
	mass <- masses[[i]]
	cols <- colnames(dat.mat) %in% mass
	dat.mat <- dat.mat[,cols]
	d <- data.frame(dat.mat,info)
	suppressMessages(d.1 <- melt(d))
	d.1$variable <- factor(d.1$variable)
	doPlot = function(vari) {
 	 d.1.sub1 = subset(d.1, variable == vari)
 	 ggbox <- ggplot(d.1.sub1, aes(x = info, y = value,fill=info)) + 
 	   geom_boxplot() +
  	   guides(fill=FALSE) +
  	   ggtitle(vari) + xlab("Class") + ylab("log10 Intensity")
	 if (length(unique(info)) < 12){
	 	ggbox <- ggbox + scale_fill_brewer(palette="RdYlBu")
	 }
   suppressMessages(ggsave(paste(Path,DF,paste(DF,"Boxplots",sep="_"),paste(paste(DF,d.1.sub1$variable,sep="_"),".jpeg",sep=""),sep="/")))
  	dev.off()
	}
	lapply(unique(d.1$variable), doPlot)
	}	
}
