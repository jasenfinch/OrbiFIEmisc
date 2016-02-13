#' boxPlot
#' @export

boxPlot <-
function(dat,cls,mass){ 
  dat <- data.frame(Intensity=dat[,mass],cls=cls)
  ggbox <- ggplot(dat,aes(x=cls,y=Intensity,fill=cls)) + 
    geom_boxplot() + 
    theme_bw() + 
    ggtitle(mass) + 
    theme(axis.text.x  = element_text(angle=90, vjust=0.5)) +
    guides(fill=FALSE) +
    xlab("Class") + 
    ylab("Intensity")
  
  if (length(unique(dat$cls)) < 12){
    ggbox <- ggbox + scale_fill_brewer(palette="RdYlBu")
  }
  return(ggbox)
}
