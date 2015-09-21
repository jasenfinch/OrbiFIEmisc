featTab <- function(fs.stats,fs.pval)
{
  methods <- names(fs.stats)
  fs.tab <- lapply(methods,function(x,s,p){
  	if(length(p)>0){
    	stats <- data.frame(feature=rownames(s),score=s[,grep(x,colnames(s))],pvalue=p[,grep(x,colnames(p))],stringsAsFactors = F)
  	} else {
  		stats <- data.frame(feature=rownames(s),score=s[,grep(x,colnames(s))],stringsAsFactors = F)
  	}
    stats <- stats[order(stats$score,decreasing=T),]
    return(stats)
  },s=fs.stats,p=fs.pval)
  return(fs.tab)
}