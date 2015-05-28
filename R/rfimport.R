rfimport <-
function(fs.res,dn,sel,Path,DF,meth="rf"){  	# function to plot RF importance curves
  library(reshape2)
  fs.rank  <- lapply(fs.res, function(x) lapply(x, function(y) y$fs.rank))
  fs.ord   <- lapply(fs.rank, function(x) lapply(x, function(y) fs.agg(y)$fs.order))
  stats <- data.frame(fs.tab.1(fs.res,fs.ord,DF))
  stats <- select.method(stats,meth)
  if (length(sel) >0){
    stats <- lists.spec(stats,sel)
  }
  stats <- lists.order(stats)
  stats <- stats[,seq(2,ncol(stats),2)]
  stats <- lapply(dn,function(x){list(select.method(stats,x))})
  for (i in 1:length(stats)){
    rfimport_plot(stats[[i]],dn[i],Path,DF)
  }
}
