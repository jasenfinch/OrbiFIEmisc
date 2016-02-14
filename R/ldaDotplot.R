#' Eigenvalue dotplot using ggplot2
#' @export

ldaDotplot <- function(lda.eig,Path,DF){
  dn <- rownames(lda.eig)
	.e = environment()
	lda.eig <- t(lda.eig)
	lda.eig <- melt(lda.eig)
	bitmap(paste(Path,DF,paste(DF,"PCA_&_LDA",sep="_"),paste(DF,"LDA_Eigenvalues.bmp" ,sep="_"),sep="/"))
	lda.p.1 <- ggplot(lda.eig,aes(x=Var2,y=value,colour=Var1),environment=.e) +
						 	geom_point(size=3,aes(shape=Var1)) +
						  coord_flip() +
							ylab("Eigenvalues") +
							theme(plot.title = element_text(lineheight=.8, face="bold"),legend.title=element_blank() )  +
							scale_shape_manual(values=c(1:length(unique(lda.eig$Var1)))) +
							xlab("") +
							ggtitle(paste(DF,": LDA Eigenvalues"))						
	print(lda.p.1)
	dev.off()
}
