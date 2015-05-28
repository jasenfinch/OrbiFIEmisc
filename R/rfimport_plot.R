rfimport_plot <-
function(x,dn,Path,DF){		# Plotting function for rfimport
  if(!file.exists(paste(Path,DF,paste(DF,"Classification_&_Feature_Selection",sep="_"),sep="/"))){
    dir.create(paste(Path,DF,paste(DF,"Classification_&_Feature_Selection",sep="_"),sep="/"))
  }
	suppressPackageStartupMessages(library(ggplot2))
	stats <- data.frame(x)
	stats[,ncol(stats)+1] <- seq(1,nrow(stats)) 
	stats[,ncol(stats)] <- as.factor(stats[,ncol(stats)])
	names(stats)[ncol(stats)] <- "Rank"
	suppressMessages(stats <- melt(stats))
	names(stats)[2] <- "Pairwise"
	stats[,1] <- as.numeric(as.character(stats[,1]))
	stats <- na.omit(stats)
	stats <- stats[stats$value > 0,]
	ggline <- ggplot(stats, aes(x=Rank, y=value, colour=Pairwise,group=Pairwise)) + 
  	  geom_point(size=1.5) + xlab("Rank") + ylab("Importance Score")
	print(ggline)
	suppressMessages(ggsave(paste(Path,DF,paste(DF,"Classification_&_Feature_Selection",sep="_"),paste(DF,dn,"RFimportance_plot.jpg",sep="_"),sep="/")))
}
