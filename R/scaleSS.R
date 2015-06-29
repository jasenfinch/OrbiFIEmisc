#' to scale a vector to its sum of squares

scaleSS <-
function(x){	
  y <- x/sqrt(sum(x^2))
  return(y)
}

