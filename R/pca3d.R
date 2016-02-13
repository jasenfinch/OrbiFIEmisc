#' pca3d
#' @export

pca3d <-
function(dn,dat.all,cls,info.class) {
	library(rgl)
	colour <- seq(1,length(unique(info.class)))
	pca <- lapply(dn, function(x) {
  	pca  <- pcaComp(dat.all[[x]], scale=F, pcs=1:3)
    scores <- cbind(pca$scores, y=cls,type=rep(x, nrow(dat.all[[x]])))
    list(scores=scores, vars=pca$vars)
  })
  names(pca) <- dn
  pca.scores <- do.call(rbind, lapply(pca, function(x) x$scores))
  pca.vars   <- do.call(rbind, lapply(pca, function(x) x$vars))
	pca.3d <- data.frame(pca.scores$type,pca.scores$y,pca.scores$PC1,pca.scores$PC2,pca.scores$PC3)
	## Split this into separate matrices
	pca.pos <- subset(pca.3d, pca.scores.type=="Positive_Mode")
	pca.neg <- subset(pca.3d, pca.scores.type=="Negative_Mode")
	## Interactive 3d plot for each matrix
	open3d()
	pca.i.pos <- plot3d(pca.pos$pca.scores.PC2, pca.pos$pca.scores.PC3, pca.pos$pca.scores.PC1, col=colour, 
		size=1, type = "s", 
		xlab="PC2", ylab="PC3", zlab="PC1", main= "PCA Pos")
  
	open3d()
	pca.i.neg <- plot3d(pca.neg$pca.scores.PC2, pca.neg$pca.scores.PC3, pca.neg$pca.scores.PC1, col=colour, 
		size=1, type = "s", 
		xlab="PC2", ylab="PC3", zlab="PC1", main= "PCA Neg")
	##For legend
    X11()
	colo <- seq(1,length(unique(info.class)))
	leg <- as.character(unique(info.class))
	plot.new()
	legend("center", leg, cex=1.0, bty="n",
		col=colo,pch=19)
}
