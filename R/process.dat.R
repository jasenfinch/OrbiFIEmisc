process.dat <-
function(peak.mat,cls,bat){
	dat.all <- lapply(peak.mat, function(x){
     dat <- x
     if (length(out_id)>0){
   	 	dat <- dat[-out_id,,drop=F]
     }
     if (length(out_idx)>0){
     	dat <- dat[-out_idx,,drop=F]
     }
   	 dat <- preproc.sd(dat, cls)      ## remove zero or constant signals
   	 dat <- mean.shift(dat, bat)
   	 dat <- preproc(dat, method=c("log10","TICnorm"))
  })
	return(dat.all)
}
