#' FIE-HRMS data pre-processing
#' @name preProc
#' @description Pre-process FIE-HRMS data suitable for downstream analysis.
#' @param dat A data.frame 
#' @param cls A vector denoting the class of each observation in the data.frame dat.
#' @param proportion The proportion threshold between 0 and 1, above which a varible must contain at least one class.
#' @param add A numeric value for addition to the matrix prior to log10 transformation.
#' @return A pre-processed data.frame
#' @details Includes TIC normalisation, class occupancy filtering and log10 transformation in that order.
#' @author Jasen Finch
#' @export

preProc <- function(dat,cls,proportion,add=1){
  dat <- TICnorm(dat)
  dat <- occDrop(dat,cls,proportion)
  dat <- log10(dat+add)
  return(dat)
}