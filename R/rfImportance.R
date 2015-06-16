#' function to plot RF importance curves

rfImportance <-
function(fs.res,sel,Path,DF,meth="rf",HPC_mode=F){  	
  stats <- fsTab(fs.res)
  stats <- lapply(stats,selectMethod,meth=meth)
  if (length(sel) >0){
    stats <- lapply(stats,listsSpec,sel=sel)
  }
  stats <- lapply(stats,listsOrder)
  stats <- lapply(stats,function(x){return(x[,seq(2,ncol(x),2)])})
  for (i in 1:length(stats)){
    rfImportancePlot(stats[[i]],names(fs.res)[i],Path,DF,HPC_mode=HPC_mode)
  }
}
