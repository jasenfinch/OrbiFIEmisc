#' Choose only significant features.
#' @export

listsSig <-
function(stats.3,thres,meth){
  s <- seq(1, length(names(stats.3)), 2)
  stats.4 <- NULL
  for (i in s){
    n <- data.frame(stats.3[,i:c(i+1)])
    n[,1] <- as.character(n[,1])
    n[,2] <- as.numeric(as.character(n[,2]))
	  a <- n[,2]
 	  if(meth=="anova"|meth=="welch"|meth=="kruskal"){
		  a[a > thres] <- NA
 	  }else{
 		  a[a < thres] <- NA
 	  }
	  n[,2] <- a
	  b <- n[,1]
	  b[is.na(a)] <- NA
	  n[,1] <- b
    for (x in 1:2){
      stats.4[names(n[x])] <- n[x]}
  }
  stats.4 <- data.frame(stats.4)
  return(stats.4)
}
