##this script uses the mixture of normals from Frey and Raftery 2002


galileo2mclust <- function(affinity, k=10, ...){
  require(mclust)
  x <- affinity$data
  G <- k
  out <- Mclust(x, G=G, ...)$classification
  return(out)
}
