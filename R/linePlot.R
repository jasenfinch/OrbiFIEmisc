#' linePlot
#' @export

linePlot <-
function(dat,h.group,v.group,mass){
  dat <- data.frame(Intensity=dat[,mass],h.group=h.group,v.group=v.group,stringsAsFactors = F)
  dat <- summaryDat(dat,'Intensity',c('h.group','v.group'))
  groups <- strsplit(as.character(dat$groups),'-')
  h.group <- sapply(groups,function(x){return(x[1])})
  v.group <- sapply(groups,function(x){return(x[2])})
  dat <- data.frame(h.group=h.group,Treatment=v.group,dat)
  ggline <- ggplot(dat, aes(x=h.group, y=Mean, colour=Treatment,group=Treatment)) + 
    geom_errorbar(aes(ymin=Mean-CI, ymax=Mean+CI), width=.1,position=position_dodge(.1)) +
    geom_line(position=position_dodge(.1)) +
    theme_bw() +
    geom_point(position=position_dodge(.1)) + xlab("Hours post inoculation") + ylab("log10 Intensity") +
    ggtitle(mass)
  return(ggline)
}


