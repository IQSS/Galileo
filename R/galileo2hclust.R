##this function will be called to allow for hierarchical clustering
##from hclust

galileo2hclust<- function(affinity, metric='euclidean', link='complete',k=10,...){
  require(amap)
  d <- affinity$data
  method <- metric
  link <- link
  out <- hcluster(d, method=method, link=link,...)
  if(is.null(k)==F){
    out <- cutree(out, k=k)}
  return(out)
	}
