#' Plot peak intensities for a 2dp mass bins for all classes
#' @export

plotBin <-
function(master_mat,masses,Path,DF,HPC_mode=F){
  for (i in 1:length(masses)){
		mass <- masses[[i]]
		mass.1 <- masses[[i]]
		if (grepl("p",mass[1])==T){
			mass <- gsub("p","",mass)
		}
		if (grepl("n",mass[1])==T){
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
 	    if (HPC_mode==T){
		  		bitmap(paste(Path,DF,paste(DF,"Bin_Plots",sep="_"),paste(DF,paste("Peaks_",mass.1[x],".bmp",sep=""),sep="_"),sep="/"))
		  } else {
		  		png(paste(Path,DF,paste(DF,"Bin_Plots",sep="_"),paste(DF,paste("Peaks_",mass.1[x],".png",sep=""),sep="_"),sep="/"))
		  }
      p <- ggplot(mat.1,aes(x=Var1,y=0,xend=Var1,yend=value)) +
        geom_segment() +
        theme_bw() +
        theme(axis.text.x  = element_text(angle=90, vjust=0.5)) +
        facet_wrap(~Var2) +
        ggtitle(mass.1[x]) +
        xlab("m/z") + 
        ylab("Intensity")
			print(p)
			dev.off()
		}
	}
}
