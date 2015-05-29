

removeSig <-
function(peak.mat,out_path){ # remove top ranked signals
	fs_out <- read.table(file=out_path, sep=",",
                     na.strings = "", stringsAsFactors=F, head=T)
  fs_out <- lapply(as.list(fs_out), function(x) x[!is.na(x)])
  fs_out_pos <- fs_out[[1]]
  idx  <- !(colnames(peak.mat$pos) %in% fs_out_pos)
  peak.mat$pos <- peak.mat$pos[,idx,drop=F]
  fs_out_neg <- fs_out[[2]]
  idx  <- !(colnames(peak.mat$neg) %in% fs_out_neg)
  peak.mat$neg <- peak.mat$neg[,idx,drop=F]
	return(peak.mat)
}
