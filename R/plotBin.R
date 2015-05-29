#' Plot peak intensities for a 2dp mass bins for all classes

plotBin <-
function(master_mat,masses,Path,DF){
  for (i in 1:length(masses)){
		mass <- masses[[i]]
		mass.1 <- masses[[i]]
		if (i==1){
			mass <- gsub("p","",mass)
		}
		if (i==2){
			mass <- gsub("n","",mass)
		}
		mass <- as.numeric(mass)
		mat <- master_mat[[i]]
		for (x in 1:length(mass)){
			mat.1 <- mat[which(mat[,3]==mass[x]),]
			mat.1 <- mat.1[,4:ncol(mat.1)]
			rownames(mat.1) <- mat.1[,1]
			mat.1 <- mat.1[,-1]
			suppressMessages(mat.1 <- melt(mat.1))
      p <- ggplot(mat.1,aes(x=Var1,y=value)) +
        geom_bar(stat="identity") +
        facet_wrap(~Var2) +
        ggtitle(mass.1[x]) +
        xlab("m/z") + 
        ylab("Intensity")
			print(p)
      suppressMessages(ggsave(paste(Path,DF,paste(DF,"Bin_Plots",sep="_"),paste(DF,paste("Peaks_",mass.1[x],".jpeg",sep=""),sep="_"),sep="/"),scale=2))
		}
	}
}
