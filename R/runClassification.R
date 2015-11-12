runClassification <- function(dat,cl.method, cl.pars,...) {
  results <- lapply(cl.method, function(m,p,dat) {
    cat("\nClassifier = :",m,"\n"); flush.console()
    val <- accest(dat[,1:ncol(dat)-1],dat[,ncol(dat)], clmeth=m, pars=cl.pars)
    acc <- val$acc
    auc <- ifelse(!is.null(val$auc), val$auc, NA)
    mar <- ifelse(!is.null(val$mar), val$mar, NA)
    res <- c(acc=acc, auc=auc, mar=mar)
    res <- round(res, digits=3)
    return(res)
  },p=cl.pars,dat=dat)
  names(results) <- cl.method
  results <- do.call(rbind, results)
}