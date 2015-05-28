logratio <-
function(x,info) {
  me <- aggregate(x,by=list(info$genos),mean)
  rownames(me) <- me$Group.1
  me$Group.1 <- NULL
  tme <-t(me)
  inf <- tme[,seq(1,8,2)]
  mock <- tme[,seq(2,8,2)]
  lrmat <- log2(inf/mock)
  rownames(lrmat) <- rownames(tme)
  colnames(lrmat) <- sort(unique(info$tp))
  return(lrmat)
}
