galileo2neuralgas<- function(affinity, k=10, dist='euclidean',...){
  if(!('flexclust' %in% .packages(all=TRUE)))
    install.packages('flexclust')
  require(flexclust)

  x <- affinity$data
  k <- k
  dist <- dist
  out <- cclust(x, k, dist, method='neuralgas')
  out <- out@cluster
  return(out)
}
	
