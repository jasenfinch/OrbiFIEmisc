#' return lists of explanatory masses for each pairwise

listsPrepSig <-
function(fs.res,DF,meth,sel,cut,dn){
	fs.order  <- lapply(fs.res, function(x) lapply(x, function(y) y$fs.order))
	fs.rank  <- lapply(fs.res, function(x) lapply(x, function(y) y$fs.rank))
	fs.ord   <- lapply(fs.rank, function(x) lapply(x, function(y) fs.agg(y)$fs.order))
	stats <- data.frame(fs.tab.1(fs.res,fs.ord,DF))
	stats <- selectMethod(stats,meth)
  if(length(sel)>0){
		stats <- listsSpec(stats,sel)
	}
	stats <- listsOrder(stats)
	stats <- listsSig(stats,cut,meth)
	stats <- stats[,seq(1,ncol(stats),2)]
	stats <- lapply(dn,function(x){list(selectMethod(stats,x))})
	#names(stats) <- dn
	return(stats)
}
