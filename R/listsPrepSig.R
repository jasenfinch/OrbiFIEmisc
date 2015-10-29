#' return lists of explanatory masses for each pairwise

listsPrepSig <-
function(fs.res,meth,cut,sel=NULL){
  stats <- lapply(fs.res,as.data.frame,stringsAsFactors=F)
  stats <- lapply(stats,function(x,m){x[grep(m,names(x))]},m=meth)
	if(length(sel)>0){
	stats <- lapply(stats,listsSpec,sel=sel)
	}
  stats <- lapply(stats,function(x){
    if (length(grep('pvalue',names(x)))>0){
      x <- x[,-grep('pvalue',names(x))]
    }
    return(x)
  })
	stats <- lapply(stats,listsSig,thres=cut,meth=meth)
	stats <- lapply(stats,function(x){return(x[,seq(1,ncol(x),2)])})
	names(stats) <- names(fs.res)
	return(stats)
}
