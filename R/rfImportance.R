#' function to plot RF importance curves
#' @export

rfImportance <-
function(fs.res,sel,Path,DF,meth="rf"){  	
  stats <- lapply(fs.res,as.data.frame,stringsAsFactors=F) 
  stats <- lapply(stats,function(x,m){x[grep(m,names(x))]},m=meth)
  if (length(sel) >0){
    stats <- lapply(stats,listsSpec,sel=sel)
  }
  stats <- lapply(stats,function(x){return(x[grep('score',names(x))])})
  for (i in 1:length(stats)){
    rfImportancePlot(stats[[i]],names(fs.res)[i],Path,DF)
  }
}
