##this is the wrapper for hybridHclust

galileo2hybridHclust<- function(affinity, trace=T,k=NULL, ...){
  if(!('hybridHclust' %in% .packages(all=TRUE)))
    install.packages('hybridHclust')
  require(hybridHclust)
	
  x <- affinity$data
  trace <- trace
  k <- k
  out <- hybridHclust(x, trace)
  if (is.null(k)==F){
    out <- cutree(out, k=k)}  
  return(out)
}

