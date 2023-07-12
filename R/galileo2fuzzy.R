##this file compute fuzzy k-means clustering

galileo2fuzzy<- function(affinity, k=10, ...){
  if(!('cluster' %in% .packages(all=TRUE)))
    install.packages('cluster')

  require(cluster)
  x <- 1- affinity$cosine
  x <- as.dist(x)
  k <- k
  out <- fanny(x, k, ...)$clustering
  return(out)
	}
