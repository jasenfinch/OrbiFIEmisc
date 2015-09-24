#' Total Ion Count Normalisation.
#' @name TICnorm
#' @description Normalise FIE-HRMS spectra to their total ion count.
#' @param dat A data.frame.
#' @return A TIC normalised data.frame.
#' @author Jasen Finch
#' @export

TICnorm <- function(dat){
  dat <- dat/rowSums(dat)
  return(dat)
}