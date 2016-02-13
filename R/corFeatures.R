#' correlation dendrogram of explanatory features based on all the data.
#' @export

corFeatures <-
function(dat.all,masses){	
	for (i in 1:length(dat.all)){
		dat <- dat.all[[i]]
		mas <- colnames(dat) %in% masses[[i]]
		dat.1 <- dat[,mas]
		cor.1 <- cor(dat.1) 
		hc  <- hclust(as.dist(1 - cor.1))
 		plot(hc, hang=-1,sub="", ylab="1 - correlation", xlab="Features",cex=0.6)
		abline(h=1-0.9, col="red")
	}
}
