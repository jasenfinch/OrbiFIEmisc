#' SFthresh
#' @import plyr
#' @import metRF
#' @export

SFthresh <- function(fs.res,Forest_K,alpha=0.01,sel=NULL){
  stats <- lapply(fs.res,as.data.frame,stringsAsFactors=F)
  stats <- lapply(stats,function(x){x[grep('rf2',names(x))]})
  if (length(sel) >0){
    stats <- lapply(stats,listsSpec,sel=sel)
  }
  stats <- ldply(stats,data.frame,.id='Mode')
  Forest_K <- lapply(Forest_K,as.data.frame)
  Forest_K <- ldply(Forest_K,data.frame,.id='Mode')
  if (length(sel) >0){
    Forest_K <- Forest_K[,c(1,unlist(lapply(sel,function(x,Forest){grep(x,colnames(Forest))},Forest=Forest_K)))]
  }
  stats <- lapply(unique(stats$Mode),function(x,stats,Forest_K,alpha){
    stats <- stats[which(stats$Mode==x),]
    Forest_K <- Forest_K[which(as.character(Forest_K$Mode)==x),]
    stats <- lapply(colnames(Forest_K)[-1],function(y,stats,Forest_K,alpha){
      thresh <- selectionFrequencyThreshold(nrow(stats),Trees=1000,K = Forest_K[1,y],alpha=alpha)
      stats <- stats[,grep(y,names(stats))]
      stats <- stats[which(stats[,2] >= thresh$SFT),1]
      return(stats)
    },stats=stats,Forest_K=Forest_K,alpha=alpha)
    names(stats) <- names(Forest_K)[-1]
    return(stats)
  },stats=stats,Forest_K=Forest_K,alpha=alpha)
  names(stats) <- Forest_K$Mode
  stats <- lapply(stats,function(x){as.character(sort(unique(unlist(x))))})
  return(stats)
}