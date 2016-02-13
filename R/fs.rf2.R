#' Random Forest feature selection method for returning selection frequencies
#' @name TICnorm
#' @description Normalise FIE-HRMS spectra to their total ion count.
#' @param train training set data
#' @param cls training set class labels
#' @return A list containing selection frequency feature selection results
#' @author Jasen Finch
#' @import metRF
#' @export



`fs.rf2` <- function(train,cls,...)
  {
    set.seed(1234)
    model <- randomForest(train, cls, keep.forest = TRUE,proximity = TRUE, importance = TRUE,...)
    Imp <- Importance(model, train)
    kval <- binaryTests(model)
  
    meas <- Imp$SF$Frequency
    
    fs.rank <- rank(-meas, na.last=T, ties.method="random")
    fs.order <- order(fs.rank, na.last=T)
    
    
    names(fs.rank) <- Imp$SF$Feature
    nam <- names(Imp$SF$Feature)
    if (!is.null(nam))
      fs.order <- noquote(nam)
    
    res <- list(fs.rank=fs.rank, fs.order=fs.order, stats=Imp$SF[,1:2],Forest_K=kval)
    return(res)
  }