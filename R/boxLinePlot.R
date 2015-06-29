boxLinePlot <-
function(x,cls,Path,DF,dn,masses,type,h.group=NULL,v.group=NULL,HPC_mode=F){ # Plotting function for box_line
	dat.mat <- data.frame(x)
	if (type=="bl"){
		d <- data.frame (x,cls,h.group,v.group)
		d.1 <- melt(d)
		d.1$variable <- factor(d.1$variable)
		d.1.summ <- summaryDat(d.1, measurevar="value", groupvars=c("cls","h.group","v.group","variable"))
		names(d.1.summ)[3] <- "Treatment"
		pd <- position_dodge(.1)

		# Function for plotting 
		doPlot = function(vari) {
 	  	d.1.sub1 = subset(d.1, variable == vari)
 	 		ggbox <- ggplot(d.1.sub1, aes(x = cls, y = value,fill=cls)) + 
 	 			geom_boxplot() +
  			guides(fill=FALSE) + ggtitle(vari) + xlab("Class") + ylab("log10 Intensity")
 	  	if(length(unique(cls)) < 11){
 	  		ggbox <- ggbox + scale_fill_brewer(palette="RdYlBu")
 	  	}
 	 		d.1.sub2 = subset(d.1.summ, variable == vari)
  		ggline <- ggplot(d.1.sub2, aes(x=h.group, y=value, colour=Treatment,group=Treatment)) + 
  	 	 geom_errorbar(aes(ymin=value-ci, ymax=value+ci), width=.1,position=pd) +
  	 	 geom_line(size=0.1,position=pd) +
  	 	 geom_point(position=pd) + xlab("Hours post inoculation") + ylab("log10 Intensity")
  		 if (HPC_mode==T){
		  		bitmap(paste(Path,DF,paste(DF,"Boxplots",sep="_"),paste(paste(DF,vari,sep="_"),".bmp",sep=""),sep="/"))
		   } else {
  		    jpeg(paste(Path,DF,paste(DF,"Boxplots",sep="_"),paste(paste(DF,vari,sep="_"),".jpeg",sep=""),sep="/"))
		   }
  		 grid.arrange(ggbox,ggline)
  	 	 dev.off()
	}
	lapply(unique(d.1$variable), doPlot)
	} 
	if (type=="b"){
		d <- data.frame (x,cls)
		d.1 <- melt(d)
		d.1$variable <- factor(d.1$variable)
		doPlot = function(vari) {
		  if (HPC_mode==T){
		  		bitmap(paste(Path,DF,paste(DF,"Boxplots",sep="_"),paste(paste(DF,vari,sep="_"),".bmp",sep=""),sep="/"))
		  } else {
  		    jpeg(paste(Path,DF,paste(DF,"Boxplots",sep="_"),paste(paste(DF,vari,sep="_"),".jpeg",sep=""),sep="/"))
		  }
 	 		d.1.sub1 = subset(d.1, variable == vari)
 	 		ggbox <- ggplot(d.1.sub1, aes(x = cls, y = value,fill=cls)) + 
 	   	geom_boxplot() +
  	 	guides(fill=FALSE) + ggtitle(vari) + xlab("Class") + ylab("log10 Intensity")
 	 		if(length(unique(cls)) < 11){
 	  		ggbox <- ggbox + scale_fill_brewer(palette="RdYlBu")
 	  	}
  		print(ggbox)
  		dev.off()
	}
	lapply(unique(d.1$variable), doPlot)
	}
		
	}
