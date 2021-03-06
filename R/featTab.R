#' featTab
#' @export

featTab <- function(fs.stats,fs.pval)
{
  methods <- names(fs.stats)
  fs.tab <- lapply(methods,function(x,s,p){
  	if(ncol(p)>0){
    	stats <- data.frame(feature=rownames(s),score=s[,colnames(s) %in% x],pvalue=p[,colnames(p) %in% x],stringsAsFactors = F)
  	} else {
  		stats <- data.frame(feature=rownames(s),score=s[,colnames(s) %in% x],stringsAsFactors = F)
  	}
    stats <- stats[order(stats$score,decreasing=T),]
    return(stats)
  },s=fs.stats,p=fs.pval)
  return(fs.tab)
}