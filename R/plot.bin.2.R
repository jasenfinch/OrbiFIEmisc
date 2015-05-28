plot.bin.2 <-
function(master_mat,mode,class,bin){	# Plot peak intensities for a 2dp mass bin
	if (mode=="pos"){
		a <- master_mat[[1]]
		xyplot(a[which(a[,3]==bin),class]~a[which(a[,3]==bin),4],type="h",ylab="Intensity",xlab="m/z",main=paste(mode,bin,sep="_"),col="black")
	}
	if (mode=="neg"){
		a <- master_mat[[2]]
		xyplot(a[which(a[,3]==bin),class]~a[which(a[,3]==bin),4],type="h",ylab="Intensity",xlab="m/z",main=paste(mode,bin,sep="_"),col="black")
	}
}
