

preProcess <-
function(x,cls.1=cls,bat.1=bat,srce.1=srce,out_id.1=out_id,out_idx.1=out_idx){
	dat <- x;
	if (length(out_id.1)>0){
		dat <- dat[-out_id.1,,drop=F]
			}
	if (length(out_idx.1)>0){
  	dat <- dat[-out_idx.1,,drop=F]
			}
	dat <- preprocSD(dat, cls.1);      ## remove zero or constant signals
	if (length(unique(bat.1))>1){
	dat <- meanShift(dat, bat.1)
	}
	dat <- preproc(dat, method=c("log10","TICnorm"));
}
