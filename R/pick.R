pick <-
function (z,b) {
  x <- NULL
  for (i in 1:length(b)) {
    x[b[i]] <- z[which(names(z)==b[i])]
  }
  x <- data.frame(x)
  return(x)
}
