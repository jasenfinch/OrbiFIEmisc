featTab <- function(x)
{
  x <- as.matrix(x)
  if (is.null(rownames(x)))
    rownames(x) <- paste("R", 1:nrow(x),sep="")
  if (is.null(colnames(x)))
    colnames(x) <- paste("C", 1:ncol(x),sep="")
  
  fs <- rownames(x)
  rank.tab <- lapply(as.data.frame(x), function(x){
    fs.order <- order(x,decreasing=T, na.last=T)
    fs.rank  <- order(fs.order)
    df <- data.frame(rank=fs.rank, feature=fs, value=x)
    rownames(df) <- 1:nrow(df)
    df
  })
  
  rank.tab  <- do.call("cbind",rank.tab)
  rank.tab
}