fsTab <- 
  function(fs.res){
	fs <- lapply(fs.res,function(x){return(lapply(x,function(y){return(y["fs.tab"])}))})
	fs <- lapply(fs,function(x){return(lapply(x,function(y){return(y[[1]][order(y[[1]][,3],decreasing=T),2:3])}))})
	fs <- lapply(fs,as.data.frame)
	return(fs)
}