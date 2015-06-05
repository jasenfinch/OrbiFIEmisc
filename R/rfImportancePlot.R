# Plotting function for rfImportance

rfImportancePlot <-
function(x,dn,Path,DF,HPC_mode=F){		
	stats <- data.frame(x)
	stats[,ncol(stats)+1] <- seq(1,nrow(stats)) 
	stats[,ncol(stats)] <- as.factor(stats[,ncol(stats)])
	names(stats)[ncol(stats)] <- "Rank"
	suppressMessages(stats <- melt(stats))
	names(stats)[2] <- "Pairwise"
	stats[,1] <- as.numeric(as.character(stats[,1]))
	stats <- na.omit(stats)
	stats <- stats[stats$value > 0,]
	if (HPC_mode==T){
	  bitmap(paste(Path,DF,paste(DF,"Classification_&_Feature_Selection",sep="_"),paste(DF,dn,"RFimportance_plot.bmp",sep="_"),sep="/"))
	} else {
	  jpeg(paste(Path,DF,paste(DF,"Classification_&_Feature_Selection",sep="_"),paste(DF,dn,"RFimportance_plot.jpeg",sep="_"),sep="/"))
	}
	ggline <- ggplot(stats, aes(x=Rank, y=value, colour=Pairwise,group=Pairwise)) + 
  	  geom_point(size=1.5) + xlab("Rank") + ylab("Importance Score")
	print(ggline)
  dev.off()
}
