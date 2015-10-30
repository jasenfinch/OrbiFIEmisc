#' Borda count of explanatory feature lists.

bordaCount <-
function (lists){
	borda <- NULL
	for (y in 1:length(lists)){
		uni <- NULL
		list.1 <- lists[[y]]
		for (i in 1:ncol(list.1)){
			lis <- as.character(list.1[,1])
			lis <- na.omit(lis)
			uni[(length(uni)+1):(length(uni)+length(lis))] <- lis
		}
		uni <- unique(uni)
		count.mat <- matrix(0,ncol=ncol(list.1),nrow=length(uni))
		rownames(count.mat) <- uni
		colnames(count.mat) <- colnames(lists)

		for (i in 1:ncol(list.1)){
			lis <- list.1[,i]
			lis <- na.omit(lis)
			lis <- data.frame(lis,rev(seq(1,length(lis))))
			for (x in 1:length(lis[,1])){
				pos <- grep(lis[x,1],rownames(count.mat))
				count.mat[pos,i] <- lis[x,2]
			}
	}
	Total <- apply(count.mat,1,sum)
	count.mat.1 <- data.frame(count.mat,Total)
	count.mat.1 <- count.mat.1[order(count.mat.1[,ncol(count.mat.1)],decreasing=TRUE),]
	names(count.mat.1)[1:ncol(list.1)] <- names(list.1)
	borda[y] <- list(count.mat.1)
	}
	return (borda)
}
