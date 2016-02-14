#' pcaLoadings
#' @export

pcaLoadings <- function(pca.loadings,Path,DF,pcs=1:3){
  dn <- unique(pca.loadings$type)
  for(i in 1:length(dn)){
    loadings <- pca.loadings[which(pca.loadings$type==dn[i]),]
    if(dn[i] == "Positive_Mode"){
      mz  <- as.numeric(gsub("p","",rownames(loadings)))
    }
    if(dn[i] == "Negative_Mode"){
      mz  <- as.numeric(gsub("n","",rownames(loadings)))
    }
    for (k in pcs){
      data <- data.frame(mz,loading=as.numeric(loadings[,k]),type=loadings$type)
      bitmap(paste(Path,DF,paste(DF,"PCA_&_LDA",sep="_"),paste(DF,dn[i],paste("PC",k,sep=""),"Loadings.bmp" ,sep="_"),sep="/"))
      print(ggplot(data,aes(x=mz,y=loading)) + 
        geom_point(colour="#3399FF",stat="identity") +
        ylab(paste("PC",k,sep="")) +
        facet_wrap(~type,ncol=2) +
        ggtitle(paste(DF," PC",k," Loadings" ,sep="")) +
        theme_bw()) 
      dev.off()
    }
  }
}