pcaLoadings <- function(pca.loadings,Path,DF,pcs=1:3,HPC_mode=F){
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
      data <- data.frame(cbind(mz,as.numeric(loadings[,k])))
      if (HPC_mode==T){
        bitmap(paste(Path,DF,paste(DF,"PCA_&_LDA",sep="_"),paste(DF,dn[i],paste("PC",k,sep=""),"Loadings.bmp" ,sep="_"),sep="/"))
      } else {
        jpeg(paste(Path,DF,paste(DF,"PCA_&_LDA",sep="_"),paste(DF,dn[i],paste("PC",k,sep=""),"Loadings.jpeg" ,sep="_"),sep="/"))
      }
      print(ggplot(data,aes(x=mz,y=V2)) + 
        geom_point(colour="Blue",stat="identity") +
        ylab(paste("PC",k,sep="")) +
        ggtitle(paste(DF," PC",k," Loadings" ,sep="")) )
      dev.off()
    }
  }
}