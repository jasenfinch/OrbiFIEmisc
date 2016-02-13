#' Two class LDA plot using ggplot2
#' @export

ldaStrip <-
function(lda.dfs,lda.eig,Path,DF,HPC_mode=F){
  dn <- unique(lda.dfs$type)
	.e = environment()
	for (i in 1:length(dn)){
	sub.lda <- subset(lda.dfs, lda.dfs$type == dn[i] )
	if (HPC_mode==T){
		bitmap(paste(Path,DF,paste(DF,"PCA_&_LDA",sep="_"),paste(DF,dn[i],"DF1","LDA.bmp" ,sep="_"),sep="/"))
	} else {
  	png(paste(Path,DF,paste(DF,"PCA_&_LDA",sep="_"),paste(DF,dn[i],"DF1","LDA.png" ,sep="_"),sep="/"))
	}
	plot.1 <- ggplot(sub.lda, aes(x = y, y = DF1, colour=sub.lda$y), environment=.e) + 
    	geom_point(size=2,aes(shape=sub.lda$y)) + #
    	#scale_colour_brewer(palette="Dark2") + 
    	facet_wrap( ~ type, ncol=2)+ 
  		scale_shape_manual(values=c(1:length(unique(sub.lda$y)))) +
    	ggtitle(paste(DF," LDA" ,sep="")) +
	  	scale_fill_hue(l=40) +
    	theme(plot.title = element_text(lineheight=.8, face="bold"),legend.title=element_blank() )  +
    	xlab(paste("")) + 
    	ylab(paste("DF1"," (Tw ",round(lda.eig[i], digits=2),")",sep=""))
  print(plot.1)
	dev.off()
	}
}
