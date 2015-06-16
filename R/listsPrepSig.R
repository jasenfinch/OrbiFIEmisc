#' return lists of explanatory masses for each pairwise

listsPrepSig <-
function(fs.res,meth,sel,cut){
	stats <- fsTab(fs.res)
	stats <- lapply(stats,selectMethod,meth=meth)
	if(length(sel)>0){
	stats <- lapply(stats,listsSpec,sel=sel)
	}
	stats <- lapply(stats,listsSig,thres=cut,meth=meth)
	stats <- lapply(stats,function(x){return(x[,seq(1,ncol(x),2)])})
	names(stats) <- names(fs.res)
	return(stats)
}
