

corrAnalysis <-
function(fs.ord,n,cutoff,Path,DF){
	ord    <- lapply(fs.ord, function(x) lapply(x, function(y) y[1:n]))
	fs.num <- lapply(ord, function(x) sapply(x, length))
	for(i in dn){
 	 dat.pair <- dat.sel(dat.all[[i]], cls, choices=pw)
  	com      <- apply(dat.pair$com, 1, paste, collapse="~")
  	for(j in com){
  	  dat.com <- dat.pair$dat[[j]]
  	  cl.com  <- dat.pair$cl[[j]]
  	  fs      <- ord[[i]][[j]]
   	 pdf(file = paste(Path,DF,paste(DF,"Correlation_Analysis",sep="_"),paste(DF,fs.num[[i]][[j]],i,j,"cluster_plot_re.pdf", sep="_"),sep="/"),
   	            onefile = T)
   	 res.cor <- corVec(dat.com, fs, cutoff,
   	                       fig.title=paste(DF,fs.num[[i]][[j]],i,j,sep="_"))
   	 dev.off()
   	 saveTab(shrinkList(unList(res.cor)),
    	            filename=paste(Path,DF,paste(DF,"Correlation_Analysis",sep="_"),paste(DF, fs.num[[i]][[j]],i,j,"fs.cor.re.csv",
      	                         sep='_'),sep="/"),
      	          firstline=paste('\nCorrelation analysis', DF,fs.num[[i]][[j]],
       	                   i,j,sep='_'))
  }
	}
}
