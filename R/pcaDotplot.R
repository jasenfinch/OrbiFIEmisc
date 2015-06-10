#' PCA variance dotplot using ggplot2

pcaDotplot <-
function(pca.vars,dn,Path,DF,HPC_mode=F){
	.e = environment()
	rownames(pca.vars) <- dn
	pca.vars <- t(pca.vars)
	pca.vars <- melt(pca.vars) 
	if (HPC_mode==T){
		bitmap(paste(Path,DF,paste(DF,"PCA_&_LDA",sep="_"),paste(DF,"PCA_Variance.bmp" ,sep="_"),sep="/"))
	} else {
  	jpeg(paste(Path,DF,paste(DF,"PCA_&_LDA",sep="_"),paste(DF,"PCA_Variance.jpg" ,sep="_"),sep="/"))
	}
	pca.p.1 <- ggplot(pca.vars,aes(x=Var2,y=value,colour=Var1),environment=.e) +
						 	geom_point(size=3,aes(shape=Var1)) +
						  coord_flip() +
							ylab("Percentage") +
							theme(plot.title = element_text(lineheight=.8, face="bold"),legend.title=element_blank() )  +
							scale_shape_manual(values=c(1:length(unique(pca.vars$Var1)))) +
							xlab("") +
							ggtitle(paste(DF,": PCA proportion of variance"))						
	print(pca.p.1)
	dev.off()
}