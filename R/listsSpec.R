#' Select specific feature lists

listsSpec <-
function (stats.3,sel){
  stats.4 <- NULL
  nam <- names(stats.3)
  for (i in 1:length(sel)){
    c <- grep(sel[i],nam,fixed=TRUE)
    na <- data.frame(stats.3[,c])
    for (x in 1:length(names(na))){
      stats.4[names(na[x])] <- na[x]
    }
  }
  stats.4 <- data.frame(stats.4)
  stats.4 <- data.frame(stats.4[,order(names(stats.4))])
  return(stats.4)
}
