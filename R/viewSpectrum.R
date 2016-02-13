#' view zoom able spectrum. double click to zoom out. 2 single clicks over range to zoom in.
#' @export

viewSpectrum <-
function(mat,class,mode,type="l"){ 
	if (mode =="pos"){
		dat <- mat[[1]]
		sig <- as.numeric(gsub("p","",colnames(dat)))
	}
	if (mode =="neg"){
		dat <- mat[[2]]
		sig <- as.numeric(gsub("n","",colnames(dat)))
	}	
	X11()
	coor.x <- c(1,1)
	while(TRUE){
		if(coor.x[1]==coor.x[2]){
			plot(sig,dat[class,],type=type, xlab="m/z" ,ylab = "Intensity")
		} else {
			if (coor.x[1] > coor.x[2]){
				c <- coor.x[1]
				coor.x[1] <- coor.x[2]
				coor.x[2] <- c
			}
			plot(sig[sig>coor.x[1] & sig<coor.x[2]],dat[class,sig>coor.x[1] & sig<coor.x[2]],type=type, xlab="m/z" ,ylab = "Intensity")
		}
		coor <- locator(2)
		coor.x <- coor$x
	}
}
