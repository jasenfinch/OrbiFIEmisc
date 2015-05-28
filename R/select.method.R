select.method <-
function(stats.1,meth){ # pick out individual method from agg feature lists
	nm <- names(stats.1)
	c <- grep(meth,nm,fixed=TRUE)
	stats.2 <- data.frame(stats.1[,c])
	return (stats.2)
}
