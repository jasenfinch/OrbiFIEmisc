#' reFormatClassi
#' @export

reFormatClassi <- function(aam){ 
  aam <- lapply(aam,function(x){

    pairwises <- names(x)
    methods <- rownames(x[[1]])
    param <- colnames(x[[1]])

    columnNames <- lapply(methods, function(x,y){
    nam <- lapply(y, function(j,k){return(paste(k,j,sep='-'))},k=x)
      return(nam)
    },y=param)
    columnNames <- unlist(columnNames)

    datMat <- matrix(nrow=length(pairwises),ncol=length(columnNames))
    
    for(i in 1:length(x)){
      curPair <- x[[i]]
      datMat[i,] <- unlist(apply(curPair,1,function(x){return(list(x))}))
    }

    datMat <- data.frame(Pairwise=pairwises,datMat,stringsAsFactors = F)
    colnames(datMat)[2:ncol(datMat)] <- columnNames
    return(datMat)
  })
  return(aam)
}