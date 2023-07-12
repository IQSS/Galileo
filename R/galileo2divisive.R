##this method is for divisive hierarchical clustering

galileo2divisive<- function(affinity,k=NULL, metric='stand.euc',...){
  if(!('cluster' %in% .packages(all=TRUE)))
    install.packages('cluster')
  require(cluster)

  k <- k
  if(metric=='stand.euc'){
  x <- 1- affinity$cosine
  x <- as.dist(x)
  out <- diana(x, ...)
}
  if(metric=='euclidean'){
    x <- affinity$data
    out <- diana(x,metric='euclidean',...)}
  if(metric=='manhattan'){
    x <- affinity$data
    out <- diana(x, metric='manhattan')}
  if(is.null(k)==F){
    out <- as.hclust(out)
    out <- cutree(out, k=k)
  }
  if(is.null(k)==T){
    out <- as.hclust(out)}

  return(out)
  }
