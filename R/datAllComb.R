

datAllComb <-
function(dat.all,dn){
	idx_comb   <- list(pn=c(1:length(dn)))
	dat.all.one <- lapply(idx_comb, function(x){
	  dat  <- do.call(cbind,  {
	    tmp <- dat.all[dn[x]]
	    names(tmp) <- NULL
	    tmp
	  })
	})
	return(dat.all.one)
}
