#' function to plot RF importance curves

rfImportance <-
function(fs.res,dn,sel,Path,DF,meth="rf",HPC_mode=F){  	
  fs.rank  <- lapply(fs.res, function(x) lapply(x, function(y) y$fs.rank))
  fs.ord   <- lapply(fs.rank, function(x) lapply(x, function(y) fs.agg(y)$fs.order))
  stats <- data.frame(fs.tab.1(fs.res,fs.ord,DF))
  stats <- selectMethod(stats,meth)
  if (length(sel) >0){
    stats <- listsSpec(stats,sel)
  }
  stats <- listsOrder(stats)
  stats <- stats[,seq(2,ncol(stats),2)]
  stats <- lapply(dn,function(x){list(selectMethod(stats,x))})
  for (i in 1:length(stats)){
    rfImportancePlot(stats[[i]],dn[i],Path,DF,HPC_mode=HPC_mode)
  }
}
