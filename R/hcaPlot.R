

hcaPlot <-
function(dn,dat.all,cls,Path,DF,HPC_mode=F){
 for (i in 1:length(dn)) {
    ldamod <- nlda(dat.all[[i]],cls)
    if (HPC_mode==T){
    	bitmap(paste(Path,DF,paste(DF,"HCA",sep="_"),paste(paste(DF, paste("lda_hca",dn[i],
                    "Tw1",as.character(round(ldamod$Tw[[1]]*10, digits=0)/10),
                    "Tw2",as.character(round(ldamod$Tw[[2]]*10, digits=0)/10),
                    sep="_"),
                    sep="_"),".bmp",sep=""),sep="/"))
    } else {
    	png(filename = paste(Path,DF,paste(DF,"HCA",sep="_"),paste(paste(DF, paste("lda_hca",dn[i],
                    "Tw1",as.character(round(ldamod$Tw[[1]]*10, digits=0)/10),
                    "Tw2",as.character(round(ldamod$Tw[[2]]*10, digits=0)/10),
                    sep="_"),
                    sep="_"),".png",sep=""),sep="/"))
    }
    hca.nlda(ldamod)
    paste("Tw1",as.character(round(ldamod$Tw[[1]]*10, digits=0)/10), sep="_")
    dev.off()
  }
}
