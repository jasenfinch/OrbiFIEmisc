fsTab <- 
  function(fs.res){
  # get present methods
  methods <- colnames(fs.res[[1]][[1]][[3]])
  # return fs.tab for each pairwise
  fs <- lapply(fs.res,function(x){return(lapply(x,function(y){return(y["fs.tab"])}))})
  # split each method into separate list elements to enable ordering
  fs <- lapply(fs,function(x,m){lapply(x,function(y,meth){lapply(meth,function(me,z){return(z[[1]][,grep(me,colnames(z[[1]]))])},z=y)},meth=m)},m=methods)
  # order feature list
  fs <- lapply(fs,function(x){lapply(x,function(y){lapply(y,function(z){z[order(z[,3],decreasing=T),2:3]})})})
  # convert to data.frame
  fs <- lapply(fs,as.data.frame)
	return(fs)
}