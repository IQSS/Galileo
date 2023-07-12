##this is a wrapper for the hardclustering algorithm
## which resides in the 'flexclust' algorithm


galileo2hardcl<- function(affinity, k=10,dist='euclidean', ...){
  if(!('flexclust' %in% .packages(all=TRUE)))
    install.packages('flexclust')
  require(flexclust)

  x <- affinity$data
  k <- k
  dist <- dist
  out <- cclust(x, k, dist, method='hardcl')
  out <- out@cluster
  return(out)
}
