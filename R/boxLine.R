#' boxplot and line plotting function, masses=NULL plots all

boxLine <-
function(dat.all,cls,Path,DF,dn,masses,type="bl",h.group=NULL,v.group=NULL){
	for (i in 1:length(dat.all)){
	dat <- dat.all[[i]]
	mass <- masses[[i]]
	dat.mat <- pick(dat,mass)
  boxLinePlot(dat.mat,cls,Path,DF,dn,mass,type,h.group=h.group,v.group=v.group)
	}
}
