#' summaryDat
#' @export

summaryDat <- function(data,measurvar,groupvars,conf.interval=0.95){
  if(length(groupvars)>1){
    groups <- apply(data[,groupvars],1,paste,collapse = '-')
  } else {
    groups <- data[,groupvars]
  }
  n <- aggregate(data[,measurvar],list(groups),length)
  mean <- aggregate(data[,measurvar],list(groups),mean)
  sd <- aggregate(data[,measurvar],list(groups),sd)
  se <- sd$x/sqrt(n$x)
  ci <- se * qt(conf.interval/2 + .5, n$x-1)
  groups <- n[,-which(colnames(n)=="x")]
  res <- data.frame(groups,Mean=mean[,which(colnames(n)=="x")],SD=sd[,which(colnames(n)=="x")],SE=se,CI=ci,N=n[,which(colnames(n)=="x")])
  return(res)
}