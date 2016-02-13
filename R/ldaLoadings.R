#' ldaLoadings
#' @export

ldaLoadings <- function(lda.loadings,Path,DF,dfs=1:3,HPC_mode=F){
  dn <- unique(lda.loadings$type)
  for(i in 1:length(dn)){
    loadings <- lda.loadings[which(lda.loadings$type==dn[i]),]
    if(dn[i] == "Positive_Mode"){
      mz  <- as.numeric(gsub("p","",rownames(loadings)))
    }
    if(dn[i] == "Negative_Mode"){
      mz  <- as.numeric(gsub("n","",rownames(loadings)))
    }
    for (k in dfs){
      data <- data.frame(mz,loading=as.numeric(loadings[,k]),type=loadings$type)
      if (HPC_mode==T){
        bitmap(paste(Path,DF,paste(DF,"PCA_&_LDA",sep="_"),paste(DF,dn[i],paste("DF",k,sep=""),"Loadings.bmp" ,sep="_"),sep="/"))
      } else {
        png(paste(Path,DF,paste(DF,"PCA_&_LDA",sep="_"),paste(DF,dn[i],paste("DF",k,sep=""),"Loadings.png" ,sep="_"),sep="/"))
      }
      print(ggplot(data,aes(x=mz,y=loading)) + 
              geom_point(colour="#FF3333",stat="identity") +
              ylab(paste("DF",k,sep="")) +
              facet_wrap(~type,ncol=2) +
              ggtitle(paste(DF," DF",k," Loadings" ,sep="")) +
              theme_bw())
      dev.off()
    }
  }
}