#' LDA plots using ggplot2

ldaPlots <-
function(lda.dfs,lda.eig,DF.1,DF.2,Path,DF,cls.1=NULL,HPC_mode=F){
  dn <- unique(lda.dfs$type)
	.e = environment()
	for (i in 1:length(dn)){
		sub.lda <- subset(lda.dfs, lda.dfs$type == dn[i] )
		if (HPC_mode==T){
				bitmap(paste(Path,DF,paste(DF,"PCA_&_LDA",sep="_"),paste(DF,dn[i],DF.1,DF.2,"LDA.bmp" ,sep="_"),sep="/"))
			} else {
				jpeg(paste(Path,DF,paste(DF,"PCA_&_LDA",sep="_"),paste(DF,dn[i],DF.1,DF.2,"LDA.jpg" ,sep="_"),sep="/"))
			}
		if (is.null(cls.1)){
			plot.1 <- ggplot(sub.lda, aes(x = sub.lda[,DF.1], y = sub.lda[,DF.2], colour=sub.lda$y), environment=.e) + 
				geom_point(size=2,aes(shape=sub.lda$y)) + #
				#scale_colour_brewer(palette="Dark2") + 
				facet_wrap( ~ type, ncol=2)+ 
				scale_shape_manual(values=c(1:length(unique(sub.lda$y)))) +
				ggtitle(paste(DF," LDA" ,sep="")) +
				scale_fill_hue(l=40) +
			  theme_bw() +
				theme(plot.title = element_text(lineheight=.8, face="bold"),legend.title=element_blank() )  +
				xlab(paste(DF.1," (Tw ",round(lda.eig[i,DF.1], digits=2),")",sep="")) + 
				ylab(paste(DF.2," (Tw ",round(lda.eig[i,DF.2], digits=2),")",sep=""))
		} else {
						plot.1 <- ggplot(sub.lda, aes(x = sub.lda[,DF.1], y = sub.lda[,DF.2], colour=cls.1), environment=.e) + 
				geom_point(size=2,aes(shape=sub.lda$y)) + #
				#scale_colour_brewer(palette="Dark2") + 
				facet_wrap( ~ type, ncol=2)+ 
				scale_shape_manual(values=c(1:length(unique(sub.lda$y)))) +
				ggtitle(paste(DF," LDA" ,sep="")) +
				scale_fill_hue(l=40) +
				theme_bw() +
				theme(plot.title = element_text(lineheight=.8, face="bold"),legend.title=element_blank() )  +
				xlab(paste(DF.1," (Tw ",round(lda.eig[i,DF.1], digits=2),")",sep="")) + 
				ylab(paste(DF.2," (Tw ",round(lda.eig[i,DF.2], digits=2),")",sep=""))
		}
		print (plot.1)
		dev.off()
	}
}
