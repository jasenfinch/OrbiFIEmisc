

preProcess <-
function(x,cls=NULL,bat=NULL,out_id=NULL,out_idx=NULL,TIC_cls=NULL,procmeth=c("log10","TICnorm"),pre_SD=T,mean_shift=T){
	dat <- x;
	if (length(out_id)>0){
		dat <- dat[-out_id,,drop=F]
			}
	if (length(out_idx)>0){
  	dat <- dat[-out_idx,,drop=F]
	}
	if (pre_SD==T){
	  dat <- preprocSD(dat, cls);      ## remove zero or constant signals
	}
	if (length(unique(bat))>1 & mean_shift==T){
	  dat <- meanShift(dat, bat)
	}
	dat <- log10(dat+1)
	dat <- TICnorm(dat)
}
