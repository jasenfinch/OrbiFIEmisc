

logRatio <-
function(data,cls.treat,cls.time) {
  me <- aggregate(data,by=list(paste(cls.time,cls.treat,sep="-")),mean)
  rownames(me) <- me$Group.1
  me$Group.1 <- NULL
  uni.treat <- unique(cls.treat)
  uni.time <- unique(cls.time)
  treat.control <- list()
  for (i in 1:length(uni.treat)){
    treat.control[[i]] <- me[grep(uni.treat[i],rownames(me)),]
  }
  lrmat <- log2(treat.control[[1]]/treat.control[[2]])
  rownames(lrmat) <- uni.time
  return(lrmat)
}
