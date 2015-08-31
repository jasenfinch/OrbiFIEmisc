TICnorm <- function(x){
  x.sum <- rowSums(x)
  x <- x/x.sum
  return(x)
}