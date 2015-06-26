

preProcess <-
function(x,cls,bat,out_id,out_idx,TIC_cls=NULL){
	dat <- x;
	if (length(out_id)>0){
		dat <- dat[-out_id,,drop=F]
			}
	if (length(out_idx)>0){
  	dat <- dat[-out_idx,,drop=F]
			}
	dat <- preprocSD(dat, cls);      ## remove zero or constant signals
	if (length(unique(bat))>1){
	dat <- meanShift(dat, bat)
	}
	dat <- preproc(dat, method=c("log10","TICnorm"),y=TIC_cls);
}
