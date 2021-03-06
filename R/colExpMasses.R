#' collate all explanatory masses present in all lists. 
#' @export

colExpMasses <-
function(x){  
  z <- NULL
  x <- data.frame(x)
  for (i in 1:ncol(x)){
    y <- x[,i]
     y <- as.character(y)
    y <- na.omit(y)
    z <- c(z,y)  
  }
  z <- unique(z)
  z <- sort(z)
  return(z)
}
