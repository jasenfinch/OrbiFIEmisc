#' pick out individual method from agg feature lists
#' @export

selectMethod <-
function(stats.1,meth){
	nm <- names(stats.1)
	c <- grep(meth,nm,fixed=TRUE)
	stats.2 <- data.frame(stats.1[,c])
	return (stats.2)
}
