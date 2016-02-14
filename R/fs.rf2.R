#' Random Forest feature selection method for returning selection frequencies
#' @name TICnorm
#' @description Normalise FIE-HRMS spectra to their total ion count.
#' @param x training set data
#' @param y training set class labels
#' @return A list containing selection frequency feature selection results
#' @author Jasen Finch
#' @import metRF
#' @export



`fs.rf2` <- function(x,y,...)
  {
    set.seed(1234)
    model <- randomForest(x, y, keep.forest = TRUE,proximity = TRUE, importance = TRUE,...)
    Imp <- Importance(model, x)
    kval <- binaryTests(model)
  
    meas <- Imp$SF$Frequency
    names(meas) <- Imp$SF$Feature
    
    fs.rank <- rank(-meas, na.last=T, ties.method="random")
    fs.order <- order(fs.rank, na.last=T)
    
    
    names(fs.rank) <- names(meas)
    nam <- names(meas[fs.order])
    if (!is.null(nam))
      fs.order <- noquote(nam)
    
    res <- list(fs.rank=fs.rank, fs.order=fs.order, stats=meas,Forest_K=kval)
    return(res)
  }