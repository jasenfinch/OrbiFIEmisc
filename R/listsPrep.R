#' listsPrep
#' @export

listsPrep <-
function(fs.res,meth,sel,cut){
	stats <- lapply(fs.res,as.data.frame,stringsAsFactors=F)
	stats <- lapply(stats,function(x,m){x[grep(m,names(x))]},m=meth)
	if (length(sel) >0){
	  stats <- lapply(stats,listsSpec,sel=sel)
	}
	stats <- lapply(stats,function(x){
	  if (length(grep('pvalue',names(x)))>0){
	   x <- x[,-grep('score',names(x))]
	  }
	  return(x)
	  })
	stats <- lapply(stats,listsSig,thres=cut,meth=meth)
	stats <- lapply(stats,function(x){return(x[,seq(1,ncol(x),2)])})
	masses <- lapply(stats,colExpMasses)
	names(masses) <- names(fs.res)
	return(masses)
}
