#' pca plots using ggplot2

pcaPlots <-
function(pca.scores,pca.vars.1,PC.1,PC.2,dn,Path,DF,cls.1=NULL,HPC_mode=F){
	.e = environment()
	for (i in 1:length(dn)){
		sub.pca <- subset(pca.scores, pca.scores$type == dn[i] )
		sub.pca <- sub.pca[,c(PC.1,PC.2,"y","type")]
		if (HPC_mode==T){
				bitmap(paste(Path,DF,paste(DF,"PCA_&_LDA",sep="_"),paste(DF,dn[i],PC.1,PC.2,"PCA.bmp" ,sep="_"),sep="/"))
			} else {
				jpeg(paste(Path,DF,paste(DF,"PCA_&_LDA",sep="_"),paste(DF,dn[i],PC.1,PC.2,"PCA.jpg" ,sep="_"),sep="/"))
			}
		if (is.null(cls.1)){
			plot.1 <- ggplot(sub.pca, aes(x = sub.pca[,PC.1], y = sub.pca[,PC.2], colour=sub.pca$y), environment=.e) + 
				geom_point(size=2,aes(shape=sub.pca$y)) + #
				#scale_colour_brewer(palette="Dark2") + 
				scale_shape_manual(values=c(1:length(unique(sub.pca$y)))) + 
				facet_wrap( ~ type, ncol=2)+ 
				ggtitle(paste(DF," PCA" ,sep="")) +
				scale_fill_hue(l=40) +
				theme(plot.title = element_text(lineheight=.8, face="bold"),legend.title=element_blank() )  +
				xlab(paste(PC.1," (Var ",round(pca.vars.1[i,PC.1], digits=2),"%)",sep="")) + 
				ylab(paste(PC.2," (Var ",round(pca.vars.1[i,PC.2], digits=2),"%)",sep=""))
		} else {
			plot.1 <- ggplot(sub.pca, aes(x = sub.pca[,PC.1], y = sub.pca[,PC.2], colour=cls.1), environment=.e) + 
				geom_point(size=2,aes(shape=sub.pca$y)) + #
				#scale_colour_brewer(palette="Dark2") + 
				scale_shape_manual(values=c(1:length(unique(sub.pca$y)))) + 
				facet_wrap( ~ type, ncol=2)+ 
				ggtitle(paste(DF," PCA" ,sep="")) +
				scale_fill_hue(l=40) +
				theme(plot.title = element_text(lineheight=.8, face="bold"),legend.title=element_blank() )  +
				xlab(paste(PC.1," (Var ",round(pca.vars.1[i,PC.1], digits=2),"%)",sep="")) + 
				ylab(paste(PC.2," (Var ",round(pca.vars.1[i,PC.2], digits=2),"%)",sep=""))
		}
		print (plot.1)
		dev.off()
	}
}
