#' boxLinePlot
#' @export

boxLinePlot <-
function(masses,dat,cls=NULL,h.group=NULL,v.group=NULL,type='b'){ 
  plots <- lapply(masses, function(m,dat,cls,h,v){
    if (type=="b"){
      p <- boxPlot(dat,cls,m)
    }
    if (type=="l"){
      p <- linePlot(dat,h,v,m)
    }
    if (type=="bl"){
      b <- boxPlot(dat,cls,m) 
      l <- linePlot(dat,h,v,m)
      p <- marrangeGrob(list(b,l),ncol=1,nrow=2) 
    }
    return(p)
  },dat=dat,cls=cls,h=h.group,v=v.group)
  names(plots) <- masses
  return(plots)
}
  
  
  