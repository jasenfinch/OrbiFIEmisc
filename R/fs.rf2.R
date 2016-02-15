#' Random Forest feature selection method for returning selection frequencies
#' @name TICnorm
#' @description Normalise FIE-HRMS spectra to their total ion count.
#' @param x training set data
#' @param y training set class labels
#' @return A list containing selection frequency feature selection results
#' @author Jasen Finch
#' @import metRF
#' @export



`fs.rf2` <- function(x,y,nTree=1000,...)
  {
    set.seed(1234)
    model <- randomForest(x, y, ntree = nTree,keep.forest = TRUE,proximity = TRUE, importance = TRUE,...)
    Imp <- Importance(model, x)
    kval <- round(mean(apply(model$forest$nodestatus,2,function(x){length(which(x==1))})),0)
  
    meas <- Imp$SF
    
    fs.rank <- rank(-meas$Frequency, na.last=T, ties.method="random")
    names(fs.rank) <- meas$Feature
    
    fs.order <- order(fs.rank, na.last=T)
    
    nam <- meas$Feature[fs.order]
    if (!is.null(nam))
      fs.order <- noquote(nam)

    FPR <- sapply(sort(unique(meas$Frequency)),selectionFrequencyFPR,K=kval,Tr=nTree,Ft=nrow(meas))
    FPR.pos <- match(meas$Frequency,sort(unique(meas$Frequency)))
    for (i in 1:length(FPR)){
      FPR.pos[which(FPR.pos==i)] <- FPR[i]
    }
    
    res <- list(fs.rank=fs.rank, fs.order=fs.order, stats=meas,pval=FPR.pos)
    return(res)
  }