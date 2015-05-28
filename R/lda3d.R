

lda3d <-
function(dn,dat.all,cls,info.class,info.genos){
	 library(rgl)
	 pcalda <- lapply(dn, function(x) {
    res <- nlda(dat.all[[x]],cls)
    dfs <- as.data.frame(res$x)
    dfs <- cbind(dfs, y=cls,type=rep(x, nrow(dat.all[[x]])))
    list(dfs=dfs, eig=res$Tw)
  })
  names(pcalda) <- dn
  
  lda.dfs <- do.call(rbind, lapply(pcalda, function(x) x$dfs))
  lda.eig <- do.call(rbind, lapply(pcalda, function(x) x$eig))
  
	colour <- seq(1,length(unique(info.class)))
	lda.3d <- data.frame(lda.dfs$type,lda.dfs$y,lda.dfs$DF1,lda.dfs$DF2,lda.dfs$DF3)
	## Split this into separate matrices
	lda.pos <- subset(lda.3d, lda.dfs.type=="pos")
	lda.neg <- subset(lda.3d, lda.dfs.type=="neg")
	## Axis Lables
	pxl <- paste("DF2 (Tw ",round(lda.eig[1,2], digits=2),")",sep="") 
	pyl <- paste("DF3 (Tw ",round(lda.eig[1,3], digits=2),")",sep="") 
	pzl <- paste("DF1 (Tw ",round(lda.eig[1,1], digits=2),")",sep="")
	nxl <- paste("DF2 (Tw ",round(lda.eig[2,2], digits=2),")",sep="") 
	nyl <- paste("DF3 (Tw ",round(lda.eig[2,3], digits=2),")",sep="") 
	nzl <- paste("DF1 (Tw ",round(lda.eig[2,1], digits=2),")",sep="")    
	## Interactive 3d plot for each matrix
	open3d()
	lda.i.pos <- plot3d(lda.pos$lda.dfs.DF2, lda.pos$lda.dfs.DF3, lda.pos$lda.dfs.DF1, col=colour, 
			size=1, type = "s", 
			xlab=pxl, ylab=pyl, zlab=pzl, main= "LDA Pos") 

	open3d() 
	lda.i.neg <- plot3d(lda.neg$lda.dfs.DF2, lda.neg$lda.dfs.DF3, lda.neg$lda.dfs.DF1, col=colour, 
			size=1, type = "s", 
			xlab=nxl, ylab=nyl, zlab=nzl, main= "LDA Neg") 

	##For legend 
	X11()
	colo <- seq(1,length(unique(info.class)))
	leg <- as.character(unique(info.class))
	plot.new()
	legend("center", leg, cex=1.0, bty="n",
		col=colo,pch=19)
}
