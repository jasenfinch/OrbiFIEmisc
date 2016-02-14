#' Order importance scores of feature lists
#' @export

listsOrder <-
function(stats.2) {
  s <- seq(1, length(names(stats.2)), 2)
  stats.3 <- NULL
  meth <- c("fs.rf","fs.welch","fs.anova","fs.auc","fs.kruskal","fs.bw","fs.snr","fs.relief","fs.mi")
  for (i in s){
    n <- data.frame(stats.2[,i:(i+1)])
    n[,2] <- as.numeric(as.character(n[,2]))
    for (x in 1:length(meth)){
      meth.find <- grep(meth[x],names(n))
      if(length(meth.find)>0){
        meth.1 <- meth[x]
        break
      }
    }
    if(meth.1=="anova"|meth.1=="welch"|meth.1=="kruskal"){
      n <- n[order(n[2],decreasing=FALSE),]
    }else{
      n <- n[order(n[2],decreasing=TRUE),]
    }
    for (x in 1:2){
      stats.3[names(n[x])] <- n[x]}
  }
  stats.3 <- data.frame(stats.3)
  return (stats.3)
}
